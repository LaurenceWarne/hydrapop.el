# hydrapop.el

`hydrapop.el` is a package aiming to simplify and streamline the construction of project specific (and generic) dashboards, powered by [hydra](https://github.com/abo-abo/hydra/).

## Installation

It's not currently on any package archive, installing from source is the only option, for example using quelpa:

```elisp
(use-package hydrapop
  :quelpa (hydrapop :fetcher github :repo "laurencewarne/hydrapop")
  :bind ("C-M-c" . hydrapop-invoke))
```

## Examples

General strategy:
1. Generate a dope banner using e.g. [figlet](http://www.figlet.org/), you'll likely have to escape backslashes.
2. Set the `hydrapop-board` variable in a `.dir-locals.el` of your choice:
```elisp
;; .dir-locals.el
((nil . ((eval . (hydrapop-define-board hydrapop-project-board
		           " _           _                         
| |_ _  _ __| |_ _ __ _ _ __  ___ _ __ 
| ' \\ || / _` | '_/ _` | '_ \\/ _ \\ '_ \\
|_||_\\_, \\__,_|_| \\__,_| .__/\\___/ .__/
     |__/              |_|       |_|"
		           (list (hydrapop-projectile-column) (hydrapop-github-column))))
	     (hydrapop-board . hydrapop-project-board/body))))
```

Note `hydrapop-project-board/body` is generated by hydra and can be used globally.
3. Now `C-M-c` (or equivalently `hydrapop-invoke`) will popup your board whenever you're in a file for which the `.dir-locals.el` applies.

### This Project

```elisp
(hydrapop-define-board my-hydrapop-board
  " _           _                         
| |_ _  _ __| |_ _ __ _ _ __  ___ _ __ 
| ' \\ || / _` | '_/ _` | '_ \\/ _ \\ '_ \\
|_||_\\_, \\__,_|_| \\__,_| .__/\\___/ .__/
     |__/              |_|       |_|"
  (list (hydrapop-projectile-column) (hydrapop-github-column)))
```

![hydrapop-ex1](https://user-images.githubusercontent.com/17688577/193296424-fc5a0c54-b26f-4efe-9569-0d2b6f1e91aa.png)

### [projectile](https://github.com/bbatsov/projectile)

```elisp
(hydrapop-define-board hp-projectile-project-board
  "    ___________,
 \-'       _____|===========O
  )   _ __/
 / `./_/
|   |      
|   \         
`---'"
  (list (hydrapop-projectile-column)
        (hydrapop-github-column)
        (hydrapop-column-from-lists
         "Links"
         `("d" "Documentation"
           ,(hydrapop-browse-url "https://docs.projectile.mx/projectile/index.html"))
         `("C" "Contributing"
           ,(hydrapop-browse-url "https://docs.projectile.mx/projectile/contributing.html")))))
```

![hydrapop-ex2](https://user-images.githubusercontent.com/17688577/198323080-379a0a9a-9389-46d8-936f-e764c04e72ab.png)

Note `hydrapop-browse-url` is sugar around `browse-url`, it just returns an interactive function which in turn calls `browse-url` to save us from having to use a `lambda`.

### [Emacs](https://git.savannah.gnu.org/cgit/emacs.git)

```elisp
(hydrapop-define-board emacs-project-board
  "_    ___ _             _
_ ___ __ ___  __    _ ___
__   _     ___    __  ___
    _           ___     _
   _  _ __             _
   ___   __            _
         __           _
          _      _   _
         _      _    _
            _  _    _
        __  ___
       _   _ _     _
      _   _
    _    _
   _    _
  _
__"
  (list (hydrapop-column-from-lists
         "Links"
         `("b" "Bug Tracker"
           ,(hydrapop-browse-url "https://debbugs.gnu.org/cgi/pkgreport.cgi?bug-rev=on;package=emacs"))
         `("c" "Contributing"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Contributing.html"))
         `("C" "Customization Types"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html"))
         `("d" "Documentation Tips"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html"))
         `("e" "Elisp Coding Conventions"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html"))
         `("B" "Contributing Blog Post"
           ,(hydrapop-browse-url "https://www.fosskers.ca/en/blog/contributing-to-emacs"))
         `("h" "Emacs Homepage"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/"))
         `("f" "Standard Faces"
           ,(hydrapop-browse-url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html")))
        (hydrapop-column-from-lists
         "Commands"
         `("r" "Build and Run Emacs -Q"
           ,(hydrapop-async-shell-command-from-project-root
             "make extraclean && ./autogen.sh && ./configure && make && src/emacs -Q"))
         `("R" "Build and Run Emacs with init"
           ,(hydrapop-async-shell-command-from-project-root
             "make extraclean && ./autogen.sh && ./configure && make && src/emacs")))))
```

![hydrapop-emacs-example](https://user-images.githubusercontent.com/17688577/202437822-8e4aadf4-3d17-4cbd-b539-b720e4b99f16.png)

`hydrapop-async-shell-command-from-project-root` is similar to `hydrapop-browse-url` in that it's just convenience function to save us from having to write a lambda around `async-shell-command`.
