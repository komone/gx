{application, gx,
 [{description, "GS-Style Interface to wxErlang"},
  {vsn, "0.3"},
  {modules, [
  	gx,
    gx_map,
  	gx_registry,
  	gx_runner,
  	gx_util,
  	gx_xml
  ]},
  %% Maybe never: {mod, {gx_app, []}},
  {registered, []},
  {env, [
  	{resources, ["priv", "priv/gx"]}
  ]},
  {applications, [kernel, stdlib, wx]}
]}.
