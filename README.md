Check out a demo at https://a5308y.github.io/Methodoly

# Running the app

Compile the app with `elm-make Main.elm --output main.js`. Then run elm-reactor and open `index.html`.
Alternatively you can install elm-live via npm (`npm install -g elm-live`) and run `elm-live Main.elm --open -- --output=main.js` for hot reloading.
No need to run a backend server yet. The app uses a simple json storage. For now it's http://myjson.com/api (https://github.com/lramoth/myjson), but it could be our own Server.

# Contributing

For development it might be helpful to include the debug flag:
`elm-make Main.elm --output main.js --debug`

Use `google-closure-compiler-js main.js > minified.js` to minify the js result.
`npm install -g google-closure-compiler-js` to install the compiler.

We use http://fezvrasta.github.io/bootstrap-material-design/docs/4.0 to allow
a material design look while using elm-bootstrap to have typesafe styling I can
understand. This means we're not getting to use everything. For example
condensed list groups are hard because elm-boostrap does not allow a custom
class for list groups, but that would be necessary for a condensed list group. We can always give up type safety and right the classes manually where we really want a feature though.

Adjust bootstrap settings (like colors) by editing `bootstrap/scss/_custom.scss`.

To compile the stylesheets run `npm run build:css` in the bootstrap folder and
copy the resulting file from `bootstrap/dist/css` to the file imported in
index.html. If you didn't before you will have to run `npm install`.

Use http://material.io/tools/icons to explore icons of the robot font that can be used easily in the app.

Opaque store and opaque ids. Workflows should never access anything else than store and their own model.
Possibly at one point we'll introduce something like elm-taco.

The key components of the app are the workflows. They should be independent, be based on an already existing and tried methodology like GTD, Pomodoro, autofocus, ... and include a short description of how and why they work with a link to their theory. Why are they helpful to a human and what is their goal.
