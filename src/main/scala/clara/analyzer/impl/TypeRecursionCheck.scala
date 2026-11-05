package clara.analyzer.impl

import clara.asg.{TypeConId, TypeCons, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

import scala.annotation.tailrec

object TypeRecursionCheck {
  // A recursive type (type defs referring to each other) is supported when TypeInterpreterImpl terminates on it:
  // - every cycle goes through a tagged or boxed type: unwrapping an alias isn't remembered
  // - every cycle is contractive, i.e. goes through a tuple, a record, a function or a boxed type: otherwise the
  //   type refers to itself directly and its head unwraps forever
  // - the recursion is regular, i.e. a recursive reference has type params of the referring type as its type args:
  //   so that unwrapping reaches a finite number of types
  // Each group of type defs that are in cycles with each other (a strongly connected component) is checked on its
  // own, with an error for each rule it breaks.
  def rejectUnsupported(typeStructures: TypeStructures, cons: Seq[TypeCons.WrapperTypeCon]): An[Unit] = {
    val conIds = cons.map(_.tcId).toSet

    val referencesFrom: Map[TypeConId, Seq[Reference]] = cons.map { con =>
      con.tcId -> references(typeStructures.structure(con), guarded = false).filter(r => conIds(r.to.con.tcId))
    }.toMap

    // the cons reachable through the references of con, con itself only through a cycle
    def reachable(con: TypeCons.WrapperTypeCon): Set[TypeConId] = {
      @tailrec
      def loop(toVisit: List[TypeConId], seen: Set[TypeConId]): Set[TypeConId] = toVisit match {
        case Nil => seen
        case next :: rest =>
          val unseen = referencesFrom(next).map(_.to.con.tcId).distinct.filterNot(seen)
          loop(unseen.toList ++ rest, seen ++ unseen)
      }
      loop(List(con.tcId), Set.empty)
    }
    val reachableFrom = cons.map(con => con.tcId -> reachable(con)).toMap

    val components = cons.filter(con => reachableFrom(con.tcId)(con.tcId)).map { con =>
      cons.filter(other => reachableFrom(con.tcId)(other.tcId) && reachableFrom(other.tcId)(con.tcId))
    }.distinct

    An.seq(components.map(rejectUnsupportedComponent(_, referencesFrom))).map(_ => ())
  }

  def rejectUnsupportedComponent(
    component: Seq[TypeCons.WrapperTypeCon],
    referencesFrom: Map[TypeConId, Seq[Reference]],
  ): An[Unit] = {
    def cycleText(cycle: Seq[TypeCons.WrapperTypeCon]) = cycle.map(c => safe"`${c.name}`").safeString(" -> ")

    val aliasCycleError =
      findCycle(component.filter(_.typeDefKind == Ast.TypeDefKind.Alias), referencesFrom, _ => true).map { cycle =>
        SourceMessage(cycle.head.definedAt,
          safe"Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: ${cycleText(cycle)}"
        )
      }
    val uncontractiveCycleError =
      findCycle(component.filterNot(_.typeDefKind == Ast.TypeDefKind.Boxed), referencesFrom, !_.guarded).map { cycle =>
        SourceMessage(cycle.head.definedAt,
          safe"A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: ${cycleText(cycle)}"
        )
      }
    val irregularReferenceErrors = component.flatMap { con =>
      referencesFrom(con.tcId).
        filter(r => component.contains(r.to.con)).
        // each type argument of the referred type must be a plain type parameter of the referring type
        filterNot(r => r.to.typeArgs.forall(arg => con.typeParams.exists(arg == Types.Param(_)))).
        map { r =>
          SourceMessage(con.definedAt,
            safe"A recursive type can only refer to itself with type parameters as type arguments, not `${Types.toSource(r.to)}`"
          )
        }
    }

    An.seq((aliasCycleError.toSeq ++ uncontractiveCycleError ++ irregularReferenceErrors).map(An.error)).map(_ => ())
  }

  // a cycle among cons through the references `follow` lets through, depth first
  def findCycle(
    cons: Seq[TypeCons.WrapperTypeCon],
    referencesFrom: Map[TypeConId, Seq[Reference]],
    follow: Reference => Boolean,
  ): Option[Vector[TypeCons.WrapperTypeCon]] = {
    type Cycle = Vector[TypeCons.WrapperTypeCon]
    // a cycle found, or the cons visited without one
    type Visited = Either[Cycle, Set[TypeConId]]

    val conIds = cons.map(_.tcId).toSet
    def next(con: TypeCons.WrapperTypeCon) = referencesFrom(con.tcId).
      filter(reference => follow(reference) && conIds(reference.to.con.tcId)).map(_.to.con).distinct

    // `path` holds the cons being visited, `done` the ones without a cycle
    def visit(done: Set[TypeConId], path: Cycle, con: TypeCons.WrapperTypeCon): Visited =
      if (done(con.tcId)) Right(done)
      else if (path.exists(_.tcId === con.tcId)) Left(path.dropWhile(_.tcId !== con.tcId) :+ con)
      else next(con).foldLeft[Visited](Right(done)) { (visited, nextCon) =>
        visited.flatMap(visit(_, path :+ con, nextCon))
      }.map(_ + con.tcId)

    val nothingVisited: Visited = Right(Set.empty)
    val allVisited = cons.foldLeft(nothingVisited) { (visited, con) =>
      visited.flatMap(visit(_, Vector.empty, con))
    }

    allVisited match {
      case Left(cycle) => Some(cycle)
      case Right(_) => None
    }
  }

  // a reference to a named type in a type
  // guarded if it's inside a tuple, a record, a function or the type args of a boxed or opaque type
  case class Reference(to: Types.Wrapper, guarded: Boolean)

  // find all references to named types (which can contain recursion) in a type's structure, without expanding them
  def references(structure: Types.Type, guarded: Boolean): Seq[Reference] = structure match {
    case t: Types.Boxed => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded = true))
    case t: Types.Alias => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded))
    case t: Types.Tagged => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded))
    case Types.Opaque(_, typeArgs) => typeArgs.flatMap(references(_, guarded = true))
    case Types.Func(_, parameter, result) => references(parameter, guarded = true) ++ references(result, guarded = true)
    case Types.Record(fields) => fields.entries.flatMap { case (_, t) => references(t, guarded = true) }
    case Types.Tuple(ts) => ts.flatMap(references(_, guarded = true))
    case Types.Union(ts) => ts.flatMap(references(_, guarded))
    case Types.Intersection(ts) => ts.flatMap(references(_, guarded))
    case Types.Top | Types.Bottom | Types.Uni | Types.ErrorType | _: Types.Param | _: Types.Singleton => Nil
  }
}
