[
  {
    mode = "n";
    key = "<leader>e";
    action = ":Neotree toggle<CR>";
    options = {
      desc = "Toggle Explorer";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>,";
    action = ":BufferLineCyclePrev<CR>";
    options = {
      desc = "Switch Buffer";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>-";
    action = ":split<CR>";
    options = {
      desc = "Split horizontal";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>|";
    action = ":vsplit<CR>";
    options = {
      desc = "Split vertical";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>/";
    action = "<Plug>(comment_toggle_linewise_current)";
    options = {
      desc = "Toggle comment";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>:";
    action = "<cmd>Telescope command_history<CR>";
    options = {
      desc = "Command History";
      silent = true;
    };
  }
  # Buffer operations
  {
    mode = "n";
    key = "<leader>bb";
    action = "<cmd>Telescope buffers<CR>";
    options = {
      desc = "Switch Buffer";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>bd";
    action = ":bdelete<CR>";
    options = {
      desc = "Delete Buffer";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>bn";
    action = ":bnext<CR>";
    options = {
      desc = "Next Buffer";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>bp";
    action = ":bprevious<CR>";
    options = {
      desc = "Previous Buffer";
      silent = true;
    };
  }
  # File operations
  {
    mode = "n";
    key = "<leader>ff";
    action = "<cmd>Telescope find_files<CR>";
    options = {
      desc = "Find File";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>fg";
    action = "<cmd>Telescope live_grep<CR>";
    options = {
      desc = "Find Text";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>fr";
    action = "<cmd>Telescope oldfiles<CR>";
    options = {
      desc = "Recent Files";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>fn";
    action = ":enew<CR>";
    options = {
      desc = "New File";
      silent = true;
    };
  }
  # Git operations
  {
    mode = "n";
    key = "<leader>gg";
    action = ":LazyGit<CR>";
    options = {
      desc = "LazyGit";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gb";
    action = "<cmd>Telescope git_branches<CR>";
    options = {
      desc = "Branches";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gc";
    action = "<cmd>Telescope git_commits<CR>";
    options = {
      desc = "Commits";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gs";
    action = "<cmd>Telescope git_status<CR>";
    options = {
      desc = "Status";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gr";
    action = "<cmd>Gitsigns reset_hunk<CR>";
    options = {
      desc = "Reset";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gn";
    action = "<cmd>Gitsigns next_hunk<CR>";
    options = {
      desc = "Next";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>gp";
    action = "<cmd>Gitsigns prev_hunk<CR>";
    options = {
      desc = "Previous";
      silent = true;
    };
  }
  # LSP operations
  {
    mode = "n";
    key = "<leader>ih";
    action = "<cmd>lua vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())<CR>";
    options = {
      desc = "Toggle Inlay Hints";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>la";
    action = ":lua vim.lsp.buf.code_action()<CR>";
    options = {
      desc = "Code Action";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>ld";
    action = ":lua vim.lsp.buf.definition()<CR>";
    options = {
      desc = "Definition";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>lD";
    action = ":lua vim.lsp.buf.declaration()<CR>";
    options = {
      desc = "Declaration";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>li";
    action = ":lua vim.lsp.buf.implementation()<CR>";
    options = {
      desc = "Implementation";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>lr";
    action = ":lua vim.lsp.buf.references()<CR>";
    options = {
      desc = "References";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>lR";
    action = ":lua vim.lsp.buf.rename()<CR>";
    options = {
      desc = "Rename";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>lh";
    action = ":lua vim.lsp.buf.hover()<CR>";
    options = {
      desc = "Hover";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>lf";
    action = ":lua vim.lsp.buf.format()<CR>";
    options = {
      desc = "Format";
      silent = true;
    };
  }
  # Search operations
  {
    mode = "n";
    key = "<leader>sh";
    action = "<cmd>Telescope help_tags<CR>";
    options = {
      desc = "Help Tags";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>sk";
    action = "<cmd>Telescope keymaps<CR>";
    options = {
      desc = "Keymaps";
      silent = true;
    };
  }
  {
    mode = "n";
    key = "<leader>sc";
    action = "<cmd>Telescope commands<CR>";
    options = {
      desc = "Commands";
      silent = true;
    };
  }
  {
    mode = "v";
    key = "<leader>ce";
    action = "<cmd>CodeCompanion /explain<cr>";
    options.desc = "Explain Selection / Buffer (CodeCompanion)";
  }
  {
    mode = "v";
    key = "<leader>cl";
    action = "<cmd>CodeCompanion /lsp<cr>";
    options.desc = "Explain LSP (CodeCompanion)";
  }
  {
    mode = "v";
    key = "<leader>cf";
    action = "<cmd>CodeCompanion /fix<cr>";
    options.desc = "Fix (CodeCompanion)";
  }
  {
    mode = "v";
    key = "<leader>ct";
    action = "<cmd>CodeCompanion /tests<cr>";
    options.desc = "Generate tests (CodeCompanion)";
  }
  {
    mode = "n";
    key = "<leader>cC";
    action = "<cmd>CodeCompanion /commit<cr>";
    options.desc = "Generate commit message (CodeCompanion)";
  }
  {
    mode = "n";
    key = "<leader>cc";
    action = "<cmd>CodeCompanionChat<cr>";
    options.desc = "Chat (CodeCompanion)";
  }
  {
    mode = "n";
    key = "<leader>ct";
    action = "<cmd>CodeCompanionChat Toggle<cr>";
    options.desc = "Chat Toggle (CodeCompanion)";
  }
  {
    mode = "n";
    key = "<leader>cb";
    action = "<cmd>CodeCompanionChat #buffer<cr>";
    options.desc = "Chat with Buffer Content (CodeCompanion)";
  }
  {
    mode = "v";
    key = "<leader>ca";
    action = "<cmd>CodeCompanionActions<cr>";
    options.desc = "Add Selection to Chat Buffer (CodeCompanion)";
  }
  {
    mode = ["v" "n"];
    key = "<leader>cg";
    action = "<cmd>CodeCompanionToggle gemini<cr>";
    options.desc = "Toggle to Gemini Adapter (CodeCompanion)";
  }
  {
    mode = ["v" "n"];
    key = "<leader>ca";
    action = "<cmd>CodeCompanionToggle anthropic<cr>";
    options.desc = "Toggle to Anthropic Adapter (CodeCompanion)";
  }
  # PARA keymaps are now handled by the para.lua plugin itself via which-key
  # No need to duplicate them here
  # Typst operations
  {
    mode = "n";
    key = "<leader>tp";
    action = "<cmd>lua vim.fn.jobstart('xdg-open ' .. vim.fn.expand('%:p:r') .. '.pdf', {detach = true})<CR>";
    options = {
      desc = "Preview PDF";
      silent = true;
    };
  }
]
