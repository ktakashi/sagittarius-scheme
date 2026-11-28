---
name: document-sagittarius
description: Update Sagittarius Markdown reference or internal architecture documentation and validate selected pages with the existing gendoc renderer. Use for API references, executable examples, and VM/compiler documentation.
---

# Document Sagittarius

Read [document.instructions.md](../../instructions/document.instructions.md)
before documentation edits. For public APIs, get the exported interface with
[inspect-scheme-library](../inspect-scheme-library/SKILL.md), then verify the
implementation, representative usage, and focused tests.

1. Update the existing relevant page. Keep public references concise; internal
   architecture explanations belong under `doc/internal/`.
2. Follow the reference's section/definition/include syntax, not plain GFM
   alone. If adding a public page, expose it through its existing parent index.
3. State argument/return contracts, conditions, resource lifetime, and
   compatibility only when supported by source/tests. Separate observed
   behavior from proposed design.
4. Avoid trivial examples. Put examples longer than 15 lines under `example/`
   and include them with the repository syntax. Execute changed examples:

   ```sh
   bash .github/skills/scripts/sagittarius.sh run example/path/to/example.scm
   ```

5. Render only the changed pages first:

   ```sh
   bash .github/skills/scripts/sagittarius.sh doc doc/sagittarius.md
   ```

   The helper calls `doc/gendoc`, checks nonempty HTML, and removes temporary
   output. Failures are visible. Rendering exercises the parser/includes, but
   does not prove local anchors or Mermaid syntax are correct; check those
   separately when changed.
6. When changing reference inclusions/section structure or when required,
   build the full reference:

   ```sh
   bash .github/skills/scripts/sagittarius.sh build doc
   bash .github/skills/scripts/sagittarius.sh build online-doc
   ```

   Reconfigure for new documentation files: the documentation dependency glob
   does not use `CONFIGURE_DEPENDS`.
7. Report the rendered pages, example execution, and anything not validated.
   Do not claim a full documentation build from one rendered page.
