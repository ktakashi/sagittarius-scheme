---
name: create-user-story
description: Create a user story with requirements, tasks, and dependencies. Use when starting a new feature or enhancement.
disable-model-invocation: true
argument-hint: "Describe the feature or enhancement..."
---
# Create a user story

Use `$ARGUMENTS` or the user's feature description. This is an **explicit**
planning skill: do not invoke it for a routine fix or create planning files
unless requested.

1. Extract a short lowercase, hyphenated feature name. Inspect only the related
   library/API/tests; use [inspect-scheme-library](../inspect-scheme-library/SKILL.md)
   for a compact interface inventory.
2. Ask material scope/behavior questions in one structured interaction. Record
   confirmed decisions separately from assumptions; do not invent requirements.
3. Create `.copilot/{feature}/requirements.md` with these sections:

   | Section | Required content |
   | --- | --- |
   | User Story | As a [role], I want [capability], so that [benefit] |
   | Pre-requisites / Dependencies | Concrete external/internal prerequisites |
   | Detailed Tasks | A short actionable checklist with observable acceptance criteria |
   | Clarifications | Confirmed answers and remaining assumptions |
   | Next Steps | User review, then planning or implementation |

4. Include compatibility and error behavior when they affect the API. Specify
   verifiable outcomes, not implementation guesses or speculative versions.
5. Present the file through the structured review/approval tool. Only after
   approval, register it in `.copilot/README.md`; preserve existing entries.

Stop after producing the requested story. Do not automatically plan or
implement it, and do not create unrelated tracking files.
