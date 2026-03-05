# Project Notes

## Project
- Name: `architecture-viewer`
- Language: Clojure
- Goal: Create a high-level architecture diagram of a software project.
- Rendering: Use Quil to draw the architecture diagram (not HTML output).

## Test Case Project
- Path: `~/projects/clojure/empire/empire-2025`
- Constraint: Do not change `~/projects/clojure/empire/empire-2025` in any way. It is read-only for this effort.

## Viewer Requirements
- The viewer presents a high-level view of the subject project's architecture.
- It uses the project's `dependency-checker.edn` as a guide, but prioritizes actual dependencies between modules.
- The display is vertical, with components shown as layers.
- Each layer is rendered as a full-width rectangle.
- Modules are shown within each layer with a horizontal bias.
- Arrows connect modules between layers.
- UML arrowhead conventions:
  - Standard arrowheads indicate direct dependencies.
  - Closed isosceles triangle arrowheads indicate dependencies on abstract modules.

## Engineering Process

### BDD/TDD Workflow
- Write Given/When/Then scenarios in natural language for all new behaviors.
- Keep scenarios independent from code structure.
- See each scenario fail before implementation.
- Write custom parser/runner glue code connecting scenario language to production code.
- Follow the three laws of TDD while making each scenario pass.
- Do not use no-op, pending, or skeleton step definitions.
- Every step must execute real production code and be seen failing first.

### Code Quality
- Keep functions small; cyclomatic complexity should be no greater than five where practical.
- Decouple tests from production code via a testing API.
- Maintain test coverage in the high 90s for line and branch.
- Use available linters.
- Optimize implementation to minimize token count and reduce context-window pressure.

### Git Discipline
- Check test coverage before commit.
- Commit only when tests pass.
- Never push to git without explicit approval.

## Implementation Backlog

### MVP

1. Scaffold project
- Deliverable: runnable Clojure app using Quil.
- Files:
  - `deps.edn`: dependencies and aliases (including Quil)
  - `src/arch_view/core.clj`: CLI/entry point
  - `README.md`: run instructions

2. Load dependency-checker guidance
- Deliverable: parse `dependency-checker.edn` into internal data structures.
- Files:
  - `src/arch_view/input/dependency_checker.clj`: read/parse/validate guidance file
  - `src/arch_view/model/schema.clj`: normalized model keys and specs

3. Build actual module dependency graph
- Deliverable: source-derived graph of module dependencies.
- Files:
  - `src/arch_view/input/source_scan.clj`: discover source files and namespaces
  - `src/arch_view/input/dependency_extract.clj`: extract module-to-module dependencies
  - `src/arch_view/model/graph.clj`: graph constructors/utilities

4. Merge guidance with actual dependencies
- Deliverable: combined architecture model where actual dependencies are primary and guidance is advisory.
- Files:
  - `src/arch_view/model/merge.clj`: merge policy and conflict handling
  - `src/arch_view/model/classify.clj`: edge classification (`:direct`, `:abstract`)

5. Compute layered layout
- Deliverable: vertical layer assignment and horizontal module ordering within each layer.
- Files:
  - `src/arch_view/layout/layers.clj`: layer assignment algorithm
  - `src/arch_view/layout/order.clj`: horizontal bias ordering

6. Render with Quil
- Deliverable: Quil sketch rendering full-width layer rectangles and inter-layer module arrows.
- Files:
  - `src/arch_view/render/quil_view.clj`: drawing primitives, labels, arrows, arrowheads
  - `src/arch_view/render/theme.clj`: colors, spacing, font sizes

7. Arrowhead semantics (UML)
- Deliverable: arrowhead styles match edge type.
- Files:
  - `src/arch_view/render/quil_view.clj`: arrowhead shape rendering by edge class
- Rules:
  - `:direct` -> standard arrowhead
  - `:abstract` -> closed isosceles triangle arrowhead

8. End-to-end command
- Deliverable: one command to render architecture for the test case project.
- Files:
  - `src/arch_view/core.clj`: `-main` accepts `--project-path` and render options
  - `README.md`: documented example command

9. MVP verification
- Deliverable: minimal checks and smoke tests.
- Files:
  - `test/arch_view/input/dependency_checker_test.clj`
  - `test/arch_view/model/merge_test.clj`
  - `test/arch_view/layout/layers_test.clj`
  - `test/arch_view/render/quil_view_test.clj`

### V2

1. Better dependency extraction accuracy
- Add support for more dependency forms/macros and reduce false positives.
- Files:
  - `src/arch_view/input/dependency_extract.clj`
  - `test/arch_view/input/dependency_extract_test.clj`

2. Cycle handling and diagnostics
- Detect/report cycles and visualize them clearly.
- Files:
  - `src/arch_view/model/cycles.clj`
  - `src/arch_view/layout/layers.clj`
  - `src/arch_view/render/quil_view.clj`

3. Edge routing improvements
- Reduce crossing and overlaps for inter-layer arrows.
- Files:
  - `src/arch_view/layout/routing.clj`
  - `src/arch_view/render/quil_view.clj`

4. Interaction and controls
- Add toggles for edge types, module focus/highlight, pan/zoom.
- Files:
  - `src/arch_view/render/controls.clj`
  - `src/arch_view/render/quil_view.clj`

5. Guidance vs actual diff view
- Show mismatches between `dependency-checker.edn` intent and actual dependencies.
- Files:
  - `src/arch_view/model/diff.clj`
  - `src/arch_view/render/quil_view.clj`

6. Export and CI
- Deterministic rendering and optional image export for reports.
- Files:
  - `src/arch_view/render/export.clj`
  - `.github/workflows/ci.yml`
  - `README.md`

## Milestones
- Milestone 1 (MVP skeleton): steps 1-3
- Milestone 2 (first visible diagram): steps 4-6
- Milestone 3 (semantics + stability): steps 7-9
- Milestone 4 (V2 quality/features): V2 items 1-6
