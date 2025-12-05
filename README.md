
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pourquoi

**Why settle for one noisy answer when you can vote & verify?**

Add robust step-by-step reasoning to any base language model. Generate
diverse reasoning traces, vote on the consensus, and verify with the
same LLM or via a separate low-cost LLM.

pourquoi uses Verified Self-Consistency (VSC), a minimal ensemble
technique: generate diverse traces with a base model for reasoning,
normalize answers for consensus voting, then verify with a final LLM
verifier.

``` r
library(pourquoi)
```

``` r
prompt <- "how do i say why in French?"

pourquoi::why(
  prompt = prompt,
  reasoner = "ollama/gemma3:4b",
  verifier = "ollama/qwen3:0.6b",
  n_traces = 3
)
#> [1] "CONFIRMED ANSWER: pourquoi"
```

``` r
prompt <- "how many r's are in the word Strawberry?"

pourquoi::why(
  prompt = prompt,
  reasoner = "ollama/ministral-3:8b",
  verifier = "ollama/ministral-3:3b",
  n_traces = 5
)
#> [1] "3"
```
