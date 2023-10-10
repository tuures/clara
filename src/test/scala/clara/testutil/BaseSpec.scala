package clara.testutil

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Inside, EitherValues}

trait BaseSpec extends AnyFunSuite with Inside with EitherValues
