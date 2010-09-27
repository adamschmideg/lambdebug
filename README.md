
This is for you if you want to debug your clojure programs in the REPL.

Fetch it from clojars, including
`:dev-dependencies {"lambdebug" "0.3.3"}` in your `project.clj`.

And you are ready to go
    > (use 'lambdebug)
    > (debug '(+ 42 69))

For more details, visit the [lambdebug site](http://lambdebug.github.com).
