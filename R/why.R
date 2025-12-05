#' Why: Multi-trace reasoning with verification
#'
#' @description
#' Implements a reasoning pipeline that generates multiple solution traces
#' using a reasoner model, then verifies and selects the best response.
#' This approach improves reliability by sampling diverse reasoning paths
#' and using verification to identify the most accurate solution.
#'
#' @param prompt Character string. The question or problem to reason about.
#' @param reasoner A model object. The stronger model used for reasoning
#'   and verification (if no separate verifier is provided).
#' @param verifier A model object. Optional cheaper/faster model used for
#'   sampling traces. Defaults to the same model as `reasoner`.
#' @param n_traces Integer. Number of reasoning traces to generate.
#'   Recommended range is 4-8. Default is `5L`.
#' @param max_steps Integer. Maximum number of reasoning steps per trace.
#'   Default is `12L`.
#' @param temp_sample Numeric. Temperature parameter for sampling traces.
#'   Higher values produce more diverse outputs. Default is `0.7`.
#' @param temp_verify Numeric. Temperature parameter for verification.
#'   Lower values produce more deterministic verification. Default is `0.0`.
#'
#' @return The verified response or best solution trace (return type depends
#'   on implementation).
#' @export
why <- \(
  prompt,
  reasoner,
  verifier = reasoner,
  n_traces = 5L,
  max_steps = 12L,
  temp_sample = 0.7,
  temp_verify = 0.0
) {
  # 1. Universal reasoning prompt
  reasoning_prompt <- stringr::str_glue(
    "You are an extremely careful reasoning engine.\n",
    "Solve the task in **at most {max_steps} numbered steps**.\n",
    "Every step must be either:\n",
    "- A concrete observation/calculation you are 100% sure of, or\n",
    "- Marked with [UNCERTAIN] if you are guessing.\n",
    "After your last step, put the final answer in \\boxed{{...}}.\n",
    "Never skip logical steps. Never hallucinate.\n\n"
  )

  chat_base <- ellmer::chat(
    reasoner,
    system_prompt = reasoning_prompt,
    params = list(temperature = temp_sample)
  )

  # 2. Generate N independent traces (self-consistency)
  traces <- base::replicate(
    n_traces,
    chat_base$chat(
      stringr::str_glue("Task: {prompt}")
    ),
    simplify = FALSE
  ) |>
    stats::setNames(stringr::str_glue("trace_{seq_len(n_traces)}"))

  # 3A. Helper: robust answer extraction
  extract_answer <- function(text) {
    # 1. Anything inside \boxed{…}
    boxed <- stringr::str_extract(text, "\\\\boxed\\{(.*?)\\}", group = 1L)
    if (!base::is.na(boxed)) {
      return(stringr::str_trim(boxed))
    }

    # 2. Common “Final answer: …” patterns
    common <- stringr::str_extract(
      text,
      "(?i)(?:final answer|answer)[:\\s-]+\\s*(.+)$",
      group = 1L
    ) |>
      stringr::str_trim()
    if (!base::is.na(common)) {
      return(common)
    }

    # 3. Fallback: last non-empty line (still surprisingly robust)
    lines <- stringr::str_split(text, "\n")[[1]] |>
      stringr::str_trim() |>
      stringr::str_remove_all("^[-*>]\\s*")
    lines <- lines[lines != ""]
    utils::tail(lines, 1L)
  }

  # 3B. Normalise & vote
  answers_raw <- purrr::map_chr(traces, extract_answer)

  answers_norm <- answers_raw |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9.]", "") |> # strip punctuation
    stringr::str_replace("^\\.?(\\d+\\.?\\d*)$", "\\1") # pure numbers, clean

  vote_tbl <- tibble::tibble(
    raw = answers_raw,
    norm = answers_norm,
    trace = names(traces)
  ) |>
    dplyr::count(norm, raw, name = "votes") |>
    dplyr::arrange(dplyr::desc(.data$votes), .data$raw)

  best_norm <- vote_tbl$norm[1]
  candidates <- dplyr::filter(vote_tbl, .data$norm == best_norm)

  best_answer <- candidates |>
    dplyr::slice_max(.data$votes, with_ties = FALSE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(.data$raw)

  # 4. Verification pass (strong model, T = 0)
  verify_prompt <- stringr::str_glue(
    "Candidate answer that received the most votes ",
    "({sum(vote_tbl$votes[vote_tbl$norm == best_norm])}/{n_traces} traces):\n\n",
    "{best_answer}\n\n",
    "Original task: {prompt}\n\n",
    "Verify with maximum rigour.\n",
    "- If correct --> output exactly: CONFIRMED ANSWER: {best_answer}\n",
    "- If wrong/incomplete --> give the correct answer instead.\n",
    "Put your final answer in \\boxed{{...}}."
  )

  chat_verify <- ellmer::chat(
    verifier,
    system_prompt = stringr::str_glue(
      "Evaluate the accuracy and logic of the candidate answer ",
      "based on the original question and ground truth."
    ),
    params = list(temperature = temp_verify)
  )

  verification <- chat_verify$chat(
    verify_prompt
  )

  # 5. Final extraction (boxed > confirmed > raw)
  final_boxed <- stringr::str_extract(
    verification,
    "\\\\boxed\\{(.*?)\\}",
    group = 1L
  )
  if (!base::is.na(final_boxed)) {
    return(stringr::str_trim(final_boxed))
  }

  confirmed <- stringr::str_extract(
    verification,
    "CONFIRMED ANSWER:\\s*(.+)",
    group = 1L
  )
  if (!base::is.na(confirmed)) {
    return(stringr::str_trim(confirmed))
  }

  stringr::str_trim(verification)
}
