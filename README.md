# architecture-viewer

Clojure/Quil tool for visualizing a project's architecture as layered modules with dependency arrows.

## Scope

- Uses `dependency-checker.edn` as guidance.
- Uses actual namespace dependencies as primary truth.
- Renders vertical layers and module dependencies.
- Arrowheads:
  - standard for direct dependencies
  - closed triangle for abstract-module dependencies

## Test Workflow

1. Check Speclj structure:

```bash
clojure -M:spec-structure-check spec/
```

2. Run specs:

```bash
clojure -M:spec
```

## Run

Headless model build (no GUI):

```bash
clojure -M -m arch-view.core --project-path /path/to/project --no-gui
```

Headless export to EDN:

```bash
clojure -M -m arch-view.core --project-path /path/to/project --no-gui --out /tmp/architecture.edn
```

Open Quil viewer:

```bash
clojure -M -m arch-view.core --project-path /path/to/project
```

Open Quil viewer without expensive routing (faster startup, simpler arrows):

```bash
clojure -M -m arch-view.core --project-path /path/to/project --skip-routing
```

Example test case project:

```bash
clojure -M -m arch-view.core --project-path /Users/unclebob/projects/clojure/empire/empire-2025 --no-gui
```

## Tool Aliases

- `:spec-structure-check` Speclj static structure checker
- `:spec` Speclj test runner
- `:mutate` clj-mutate
- `:crap` crap4clj
