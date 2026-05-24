# Feature Tracking

This directory contains user stories and implementation plans for features.

## Structure

```
.copilot/
├── README.md                 # This file - feature index
└── {feature-name}/           # Feature directory
    ├── requirements.md       # User story and requirements
    └── plan.md              # Implementation plan
```

## Features

| Feature | Status | Created | Planned | Completed |
|---------|--------|---------|---------|-----------|
| [c11-modernization](c11-modernization/) | Completed | 2026-05-18 | 2026-05-18 | 2026-05-18 |
| [c11-source-modernization](c11-source-modernization/) | Completed | 2026-05-18 | 2026-05-18 | 2026-05-18 |
| [vm-performance](vm-performance/) | Completed | 2026-05-18 | 2026-05-18 | 2026-05-18 |
| [jit-compilation](jit-compilation/) | In Progress | 2026-05-18 | 2026-05-19 | - |

### JIT Compilation Status

**Current Phase**: ARM64 macOS MVP Complete

- ✅ JIT memory management (MAP_JIT, W^X protection)
- ✅ ARM64 assembler with label support
- ✅ ARM64 code generator (basic opcodes)
- ✅ VM integration with hot code detection
- ✅ Full test suite passing (237/237 tests)

**Next Steps**:
- Add more opcode support
- Add x86_64 backend
- Performance benchmarking

## Workflow

1. **Create User Story**: Use `/create-user-story` prompt to define requirements
2. **Plan Implementation**: Use `/plan-user-story` prompt to create detailed plan
3. **Implement**: Follow the plan to implement the feature
4. **Review**: Get user feedback after implementation
5. **Update**: Mark feature as completed in this README
