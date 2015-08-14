" Plugins
execute pathogen#infect()

" Tabs to spaces; indent size = 4
set tabstop=4
set shiftwidth=4
set expandtab

" Line numbers
set number

set visualbell
set nowrap

" gradle syntax highlighting
au BufNewFile,BufRead *.gradle set filetype=groovy

" Start NERDTree automatically
autocmd vimenter * NERDTree
" Close Vim if only window left open is NERD
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
" Open NERDTree with key binding
map <C-n> :NERDTreeToggle<CR>
