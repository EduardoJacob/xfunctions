
usethis::use_github()

usethis::use_readme_rmd()

# Start LM Studio if needed
minifunctions::terminal(".\\StartLMstudio.bat")
# Start Claude Code if needed
minifunctions::terminal("claude --model qwen/qwen3.5-9b")

usethis::browse_github()

