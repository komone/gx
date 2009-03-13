{application, gx,
 [{description, "GS-Style Interface to wxErlang"},
  {vsn, "0.1"},
  {modules, [
  	gx,
  	gx_registry,
  	gx_runner,
  	gxml
  ]},
  %% Maybe never: {mod, {gx_registry, []}},
  {registered, []},
  {env, [
  	{resources, "priv/gx"}
  ]},
  {applications, [kernel, stdlib]}
]}.
