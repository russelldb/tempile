{application, tempile,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	     tempile,
             tempile_app,
             tempile_sup
            ]},
  {registered, [tempile, tempile_sup]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { tempile_app, []}},
  {env, [{root, "priv/templates/"}]}
 ]}.
