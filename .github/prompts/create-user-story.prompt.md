---
description: "Create a user story with requirements, tasks, and dependencies. Use when starting a new feature or enhancement."
name: "Create User Story"
argument-hint: "Describe the feature or enhancement..."
agent: "agent"
---

# Create User Story

Create a concise user story based on the provided description.

## Process

1. **Analyze** the user's description to understand the feature scope
2. **Extract** a short feature name (lowercase, hyphenated) for the directory path
3. **Clarify** any ambiguous requirements by asking the user
4. **Create** the output file at `.copilot/{feature}/requirements.md`

## Output Format

Create the file `.copilot/{feature}/requirements.md` with the following structure:

```markdown
# {Feature Title}

## User Story

As a [role], I want [capability] so that [benefit].

## Pre-requisites / Dependencies

- List external dependencies
- List internal dependencies  
- List required knowledge or setup

## Detailed Tasks

1. [ ] Task 1 - Brief description
2. [ ] Task 2 - Brief description
3. [ ] Task 3 - Brief description
   - Sub-task if needed

## Clarifications

- Any clarified points from discussion
- Assumptions made

## Next Steps

After user approval, update `.copilot/README.md` to include this feature.
```

## Guidelines

- Keep the user story **concise** - avoid unnecessary verbosity
- Tasks should be **actionable** and **verifiable**
- Dependencies should be **specific** with versions where applicable
- Ask for clarification when requirements are ambiguous

## Post-Creation

After creating the user story:
1. Present the created `requirements.md` to the user for review
2. Address any feedback or modifications requested
3. Once approved, update `.copilot/README.md` to register this feature
