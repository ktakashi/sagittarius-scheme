Test failure
============

The tests below fail with `test-runner not initialized` message.

```
	 52 - rfc/oauth.scm (Failed)
	 53 - rfc/oauth2.scm (Failed)
	196 - util/concurrent.scm (Failed)
	216 - threads (Failed)
	218 - socket (Failed)
```

Your goal is to fix this issue.

The issue is most likely related to the continuations and parameters.

The strategy you should take but not limited is

1. Create a minimum reproducible script
2. Analyze the continuation and parameters
3. Fix the issue without breaking the semantics

NOTE:
You can run the individual test like this:

```shell
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild -Einfo test/runner.scm \
  test/tests/rfc/oauth.scm
```

IMPORTANT
---------

- After the fix, full test MUST pass
- The existing test MUST be intact


Summary Checklist
-----------------

Before executing context compaction

- [ ] Add `ai.md` to the compaction as user instruction
- [ ] Add general instruction

