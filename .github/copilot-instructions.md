# Clara Project - Copilot Instructions

## Running Tests

NEVER run tests by invoking sbt in the terminal (e.g. `./runsbt.sh test`). Starting sbt from scratch is very slow.

Instead, always use the Metals MCP tools which connect to the already-running Metals/BSP server:

- **Run a specific test class**: Use `mcp_clara-metals_test` with the fully qualified class name (e.g. `clara.ast.PosSpec`)
- **Run a specific test**: Use `mcp_clara-metals_test` with both `testClass` and `testName`
- **Compile a file**: Use `mcp_clara-metals_compile-file`
- **Compile the whole project**: Use `mcp_clara-metals_compile-full`

## Test class naming convention

Test classes are in `src/test/scala/clara/` and follow the pattern `clara.<package>.<ClassName>Spec`.
For example: `clara.parser.ParserImplsSpec`, `clara.analyzer.AnalyzerSpec`, `clara.E2eSpec`.
