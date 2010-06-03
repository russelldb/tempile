{application, tempile,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	     tempile,
             tempile_app,
             tempile_sup,
	     tempile_find
            ]},
  {registered, [tempile, tempile_sup]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { tempile_app, []}},
  {env, [{root, "priv/templates/"}, {extension, ".mustache"}]}
 ]}.
