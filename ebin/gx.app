{application, gx,
 [{description, "GS-Style Interface to wxErlang"},
  {vsn, "0.1"},
  {modules, [
  	gx,
  	gxml,
	gx_client
  ]},
  {registered, []},
  {env, [
  	{resources, "priv/gx"}
  ]},
  {applications, [kernel, stdlib]}
]}.
