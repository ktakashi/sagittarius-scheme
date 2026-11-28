---
name: plan-user-story
description: Create a detailed implementation plan for an existing user story. Use after a user story is created and approved.
disable-model-invocation: true
argument-hint: "Feature name or path to requirements.md..."
---
# Plan an approved user story

Use `$ARGUMENTS` to locate the feature or requirements path. Prefer an existing
approved `.copilot/{feature}/requirements.md`; use a supplied
`specification.md` when that is the story's source of truth. Do not duplicate
an existing specification or plan.

1. Read the approved requirements and only the related implementation,
   callers, tests, and docs. Resolve material missing decisions with a
   structured question before planning.
2. Create `.copilot/{feature}/plan.md` only when explicitly requested, with:

   | Section | Required content |
   | --- | --- |
   | Overview | Scope and compatibility constraints |
   | Detailed Plan | Dependency-ordered steps, affected files, acceptance criteria |
   | Testing Strategy | Exact focused test files/CTest selectors and necessary broader checks |
   | Code Quality Guidelines | Only constraints specific to this feature |
   | Implementation Checklist | Implementation, regressions, docs/examples, validation |
   | User Feedback | Approval gate and unresolved decisions |

3. Use Scheme SRFI-64 tests for Scheme/runtime behavior. Tests under
   `test/tests/` mirror libraries; native extension tests live in
   `ext/<name>/test.scm`. Verify actual CTest names with
   [run-test](../run-test/SKILL.md) rather than guessing.
4. Include generation/rebuild steps only for sources that need them, using
   [build-sagittarius](../build-sagittarius/SKILL.md). Include repeated tests
   for race/lifetime changes and comparable measurements for performance claims.
5. Estimate only if requested; explain assumptions instead of inventing
   precise hours. Keep the plan proportional to the feature.
6. Present the plan through the structured approval tool. After approval,
   update `.copilot/README.md` without overwriting existing feature status.

Stop at the requested plan. Do not execute it or mark implementation complete
merely because the plan was approved.
