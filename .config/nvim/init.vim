" **** Vim Plugins ****
"""""""""""""""""""""""
call plug#begin('~/.config/nvim/plugged')

" **Language plugins**
Plug 'enomsg/vim-haskellConcealPlus'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'tikhomirov/vim-glsl'
"Plug 'guns/vim-clojure-static'
Plug 'gkz/vim-ls'
Plug 'zah/nim.vim'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'tpope/vim-fireplace'
Plug 'vim-scripts/paredit.vim'
Plug 'luochen1990/rainbow'
"Plug 'parsonsmatt/intero-neovim'

" **Code completion
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'sebastianmarkow/deoplete-rust'    " Rust completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" **Apparence plugins**
Plug 'drewtempelmeyer/palenight.vim'
Plug 'itchyny/lightline.vim'

call plug#end()
filetype plugin on

" **** Plugins configuration ****
"""""""""""""""""""""""""""""""""

" Deoplete Configuration
"let g:deoplete#enable_at_startup = 1
"let g:deoplete#sources#rust#racer_binary= '/home/rafael049/.cargo/bin/racer'
"let g:deoplete#sources#rust#rust_source_path= '/home/rafael049/.cargo/src_racer/rust/src'

" Rust
let g:rustc_path = $HOME."cargo/bin/rustc"
let g:rust_conceal = 1
let g:rust_conceal_mod_path = 0
let g:rust_conceal_pub = 1

" Haskell-vim
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1
let g:haskell_backpack = 1
let g:haskell_classic_highlighting = 1

" Haskell Conceal
let hscoptions="𝐒𝐓𝐄𝐌tBQZNDC"

" Color Scheme and Font
set guifont=Roboto\ Mono\ Regular:h16
set termguicolors
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
colorscheme palenight
let g:palenight_terminal_italics=1
set background=dark

" Lightline configuration
let g:lightline = { 'colorscheme': 'palenight' }

" Rainbow Parentesis
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle

" **** Vim configuration ****
"""""""""""""""""""""""""""""
set wildmenu
set mouse=a
set encoding=utf-8
" Tabs and etc
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
set nowrap
set path+=** " To *find* work recursively

" Auto detect filetype
filetype on
" Show line number
set number relativenumber
" Enable synthax highlighting
syntax enable

" Disable search highlighting
set nohlsearch

"let &path.="/usr/include,"

"----- C & CPP ---
if (&ft=='c' || &ft=='cpp')
    set noexpandtab
endif
map <F5> :call CurtineIncSw()<CR>

"---- Haskell ----
if(&ft=='haskell')
    set expandtab
endif

if(&ft=='tex')
    setlocal spell spelllang=pt_br
    set wrap
    nmap ,m :make    
endif

"---- CoffeeScript ----
if(&ft=='coffee')
    map <F5> :make<CR>
endif

"---- Livescript ----
if(&ft=='ls')
    map <F5> :LiveScriptMake -o js/<CR>
endif

"-------------------
"----- Keymaps -----
"-------------------
" Auto close brackets
"inoremap " ""<left>
"inoremap ' ''<left>
"inoremap ( ()<left>
"inoremap [ []<left>
"inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O
noremap <C-F> :find 
noremap <C-S> :w<CR>
noremap <C-Q> :q<CR>

"Change 0 to ^
map 0 ^

"Paste from system's cliboard
nmap ,p :read ! xclip -o<CR>







"*** COC ***
""""""""""""
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
