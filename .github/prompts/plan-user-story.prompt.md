---
description: "Create a detailed implementation plan for an existing user story. Use after a user story is created and approved."
name: "Plan User Story"
argument-hint: "Feature name or path to requirements.md..."
agent: "agent"
---

# Plan User Story

Create a detailed implementation plan for an existing user story.

## Process

1. **Locate** the user story at `.copilot/{feature}/requirements.md`
2. **Analyze** the requirements and tasks from the user story
3. **Create** the implementation plan at `.copilot/{feature}/plan.md`

## Output Format

Create the file `.copilot/{feature}/plan.md` with the following structure:

```markdown
# Implementation Plan: {Feature Title}

## Overview

Brief summary of what will be implemented.

## Detailed Plan

### Phase 1: {Phase Name}

**Objective**: What this phase accomplishes

**Steps**:
1. Step description
   - Implementation details
   - Files to create/modify

### Phase 2: {Phase Name}

...continue for each phase...

## Estimation

| Phase | Effort | Notes |
|-------|--------|-------|
| Phase 1 | X hours/days | Assumptions |
| Phase 2 | X hours/days | Dependencies |
| **Total** | **X hours/days** | |

## Testing Strategy

> **Note**: This project implements tests at the Scheme level, not C-level unit tests.
> All test files are located in `test/tests/` directory.

### Scheme Tests
- Test case 1: Description
- Test case 2: Description

### Cross-component Tests
- Tests verifying interaction between multiple libraries
- Tests verifying C extensions work with Scheme runtime
- Full program execution tests (if applicable)

### Manual Verification
- Verification step 1
- Verification step 2

## Code Quality Guidelines

### Maintainability
- Code must be maintainable by human developers
- Follow existing project conventions and patterns
- Use clear, descriptive naming

### Comments
- Include concise comments for non-obvious logic
- Document public APIs and exported functions
- Avoid redundant comments that repeat the code

## Implementation Checklist

- [ ] Implementation complete
- [ ] Tests written and passing
- [ ] Code reviewed for maintainability
- [ ] Comments added where necessary
- [ ] Documentation updated if needed

## User Feedback

After implementation is complete:
1. Present the implementation to the user for review
2. Address any feedback or requested changes
3. Once approved, update `.copilot/README.md` to mark this feature as completed
```

## Guidelines

- Plans should be **detailed enough** for implementation without further clarification
- Estimations should include **assumptions** and **risks**
- Tests are written in **Scheme** (not C-level unit tests) in `test/tests/`
- Tests should cover **happy paths**, **edge cases**, and **error scenarios**
- Emphasize that the outcome must be **maintainable by humans**
- Comments should be **concise** and add value

## Post-Creation

After creating the plan:
1. Present the created `plan.md` to the user for review
2. Gather feedback and iterate if needed
3. Once approved, update `.copilot/README.md` to register the planning status
