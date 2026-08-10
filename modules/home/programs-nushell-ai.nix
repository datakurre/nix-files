{ config, lib, ... }:

let
  cfg = config.programs.nushell-ai;
in
{
  options.programs.nushell-ai = {
    enable = lib.mkEnableOption "Nushell AI helper module";

    systemPrompt = lib.mkOption {
      type = lib.types.str;
      default = "You are a helpful AI assistant.";
      description = "System prompt for the AI";
    };

    configPath = lib.mkOption {
      type = lib.types.str;
      default = "~/.config/nushell-ai/config.json";
      description = "Path to the configuration file managed outside NixOS (contains endpoint, model, and api_key).";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.nushell.extraConfig = ''
      # Ask the AI a question or process piped data.
      #
      # Usage Examples:
      #   ask-ai "What is 2+2?"
      #   ls | ask-ai "Summarize these files"
      #   ask-ai -v "Explain the theory of relativity"
      def "ask-ai" [
        prompt?: string
        --verbose (-v) # Show what data is sent and what is returned
      ] {
        let config_file = (${builtins.toJSON cfg.configPath} | path expand)
        if not ($config_file | path exists) {
          print -e $"Error: Configuration file not found at ($config_file)"
          print -e $"Please create it with the following JSON content:"
          print -e '{'
          print -e '  "endpoint": "https://opencode.ai/zen/v1/chat/completions",'
          print -e '  "model": "deepseek-v4-flash-free",'
          print -e '  "api_key": "your_api_key_here"'
          print -e '}'
          return
        }

        let ai_config = (open $config_file)
        let endpoint = ($ai_config | get --optional endpoint)
        let model = ($ai_config | get --optional model)
        let api_key = ($ai_config | get --optional api_key)

        if ($endpoint | is-empty) or ($model | is-empty) or ($api_key | is-empty) {
          print -e $"Error: Configuration file ($config_file) must contain 'endpoint', 'model', and 'api_key'."
          return
        }

        let input = if ($prompt | is-empty) { $in } else { $prompt }
        if ($input | is-empty) {
          print "Ask the AI a question or process piped data."
          print ""
          print "Usage:"
          print "  ask-ai [options] [prompt]"
          print ""
          print "Options:"
          print "  -v, --verbose    Show what data is sent and what is returned"
          print "  -h, --help       Display this help message"
          print ""
          print "Examples:"
          print "  > ask-ai \"What is 2+2?\""
          print "  > ls -l | ask-ai \"Explain this directory structure\""
          print "  > ask-ai --verbose \"How do I write a for loop in Python?\""
          return
        }

        let messages = [
          { role: "system", content: ${builtins.toJSON cfg.systemPrompt} }
          { role: "user", content: $input }
        ]

        let payload = {
          model: $model,
          messages: $messages
        }

        if $verbose {
          print -e $"Sending request to: ($endpoint)"
          print -e $"Payload:"
          print -e ($payload | to json)
        }

        let spinner_pid = if not $verbose {
          (^bash -c '{ trap - INT; while true; do for c in / - \\ \|; do printf "\r\033[2K$c Asking AI..." >&2; sleep 0.1; done; done; } & echo $!' | str trim | into int)
        } else {
          0
        }

        let response = try {
          http post --content-type application/json -H { Authorization: $"Bearer ($api_key)" } -e $endpoint $payload
        } catch { |err|
          if not $verbose {
            try { ^kill $spinner_pid }
            ^bash -c 'printf "\r\033[2K" >&2'
          }
          print -e $"Request failed: ($err.msg)"
          return
        }

        if not $verbose {
          try { ^kill $spinner_pid }
          ^bash -c 'printf "\r\033[2K" >&2'
        }

        if $verbose {
          print -e $"Response:"
          print -e ($response | to json)
        }

        if ($response | get --optional choices | is-empty) {
          print -e $"API Error: ($response)"
          return
        }

        $response | get choices.0.message.content
      }

      alias ai = ask-ai
    '';
  };
}
