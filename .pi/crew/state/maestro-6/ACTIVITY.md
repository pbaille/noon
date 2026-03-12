# maestro-6 Activity

## Last State
- Shutting down gracefully per agent-1 request
- Mid-conversation with user about noon's future direction

## Active Discussion: Doc/Test Pipeline
User agrees the doc/test approach is clumsy. We analyzed:
- 340 code blocks in doc/noon.org → generated test files via bb build-doc-tests
- Freeze/snapshot system (hash-based, opaque failures)
- Three generated artifacts from one org file
- User was surprised about generated/ dir in git — but it's actually properly gitignored

### Options Presented
1. Decouple docs from tests entirely (hand-written tests, lightweight "compiles" check for docs)
2. Keep doc-as-test but simplify (no build step, eval at test time, drop freeze hashes)
3. Move to CLJC literate files (Clerk/Clay notebooks)

### Awaiting
User's preference on org format and direction choice.

## Completed
- G0: Knowledge base bootstrapped (project-mapper-4, stopped)
- knowledge-daemon-31 running as maintainer
