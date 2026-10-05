-- herdr <-> Neovim: <C-h/j/k/l> moves across nvim splits and herdr panes.
--
-- Loads the file shipped by the herdr plugin `vim-herdr-navigation`.
-- The path has a hash that changes on every plugin update,
-- so we glob for it instead of hardcoding it.
--
-- Lives in after/plugin/ so it runs AFTER LazyVim's <C-hjkl> mappings.

local matches = vim.fn.glob(
  vim.fn.expand("~/.config/herdr/plugins/github/vim-herdr-navigation-*/editor/nvim.lua"),
  false,
  true
)

if matches and matches[1] then
  dofile(matches[1])
end
