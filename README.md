# sbt-decision-filter

Run only the ScalaTest test classes your git diff can affect.

An sbt plugin asks a decision API (TypeSafe Jev or OpenAI Decisions) whether each test class could be affected by the change, and leaves out the ones that cannot via sbt's `Tests.Filter`. When in doubt — no API key, an API error, a changed `build.sbt` — every test class runs.

## Setup

```scala
// project/plugins.sbt (pick one backend)
addSbtPlugin("com.krrrr38" % "sbt-decision-filter-jev" % "<version>")
// addSbtPlugin("com.krrrr38" % "sbt-decision-filter-openai-decision" % "<version>")
```

Supports sbt 1.x / 2.x. No changes to your tests are needed.

## Usage

```
export TYPESAFE_API_KEY=...   # or OPENAI_API_KEY for the OpenAI backend
sbt decisionFilterTest        # select, then run the selected test classes
sbt decisionFilterSelect      # select only; writes target/decision-filter/selection.json
```

Plain `sbt test` is unchanged.

## Settings

| Setting | Default |
| --- | --- |
| `decisionFilterBase` (or `$DECISION_FILTER_BASE`) | `$CI_MERGE_REQUEST_DIFF_BASE_SHA`, `origin/HEAD`, `origin/main` |
| `decisionFilterCutoff` / `decisionFilterUnsureBelow` / `decisionFilterUnsureMargin` | `2.0` / `0.5` / `1.0` |
| `decisionFilterRunAllPatterns` | `**.sbt`, `project/**`, `.jvmopts`, `.sbtopts`, `.gitlab-ci.yml` |
| `decisionFilterRedactPatterns` | common secret patterns |
| `decisionFilterConcurrency` / `decisionFilterTimeout` | `8` / `5.minutes` |
| `decisionFilterDryRun` | `false` |
| `decisionFilterJevModel` / `decisionFilterOpenAiModel` | `jev-1.13.0` / `gpt-6-luna` |
| `decisionFilterJevApiKeyEnv` / `decisionFilterOpenAiApiKeyEnv` | `TYPESAFE_API_KEY` / `OPENAI_API_KEY` |

Credentials are read from environment variables only.

## Acknowledgements

The scoring questions and gating rules are based on [mizchi/jev-test-filter](https://github.com/mizchi/jev-test-filter).

## License

[MIT](LICENSE)
