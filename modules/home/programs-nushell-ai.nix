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
      def "ask-ai" [prompt?: string] {
        let config_file = (${builtins.toJSON cfg.configPath} | path expand)
        if not ($config_file | path exists) {
          print -e $"Error: Configuration file not found at ($config_file)"
          print -e $"Please create it with the following JSON content:"
          print -e $"{"{"}"
          print -e $"  \"endpoint\": \"https://opencode.ai/zen/v1/chat/completions\","
          print -e $"  \"model\": \"deepseek-v4-flash-free\","
          print -e $"  \"api_key\": \"your_api_key_here\""
          print -e $"{"}"}"
          return
        }

        let ai_config = (open $config_file)
        let endpoint = ($ai_config | get -i endpoint)
        let model = ($ai_config | get -i model)
        let api_key = ($ai_config | get -i api_key)

        if ($endpoint | is-empty) or ($model | is-empty) or ($api_key | is-empty) {
          print -e $"Error: Configuration file ($config_file) must contain 'endpoint', 'model', and 'api_key'."
          return
        }

        let input = if ($prompt | is-empty) { $in } else { $prompt }
        if ($input | is-empty) {
          print -e "Error: No input provided. Please provide a prompt or pipe input."
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

        let response = (http post
          --content-type application/json
          -H { Authorization: $"Bearer ($api_key)" }
          -e
          $endpoint
          $payload)

        if ($response | get -i choices | is-empty) {
          print -e $"API Error: ($response)"
          return
        }

        $response | get choices.0.message.content
      }

      alias ai = ask-ai
    '';
  };
}
