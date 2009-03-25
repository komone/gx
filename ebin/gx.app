{application, gx,
 [{description, "GS-Style Interface to wxErlang"},
  {vsn, "0.1.0"},
  {modules, [
  	gx,
	gx_events,
	gx_map,
  	gx_registry,
  	gx_runner,
  	gx_xml
  ]},
  %% Maybe never: {mod, {gx_app, []}},
  {registered, [gx_components, gx_commands]},
  {env, [
  	{resources, "priv/gx"}
  ]},
  {applications, [kernel, stdlib]}
]}.
