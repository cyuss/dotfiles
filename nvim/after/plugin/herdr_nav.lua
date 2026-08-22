-- herdr <-> Neovim : <C-h/j/k/l> traverse les splits nvim ET les panes herdr.
--
-- Charge le fichier fourni par le plugin herdr `vim-herdr-navigation`.
-- Le chemin contient un hash qui change à chaque mise à jour du plugin,
-- donc on le résout au glob plutôt que de le coder en dur.
--
-- Placé dans after/plugin/ pour passer APRÈS les mappings <C-hjkl> de LazyVim.

local matches = vim.fn.glob(
  vim.fn.expand("~/.config/herdr/plugins/github/vim-herdr-navigation-*/editor/nvim.lua"),
  false,
  true
)

if matches and matches[1] then
  dofile(matches[1])
end
