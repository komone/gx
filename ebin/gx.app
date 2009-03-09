{application, gx,
 [{description, "GS-Style Interface to wxErlang"},
  {vsn, "0.1"},
  {modules, [
  	gx,
  	gxml
  ]},
  % no mod definition for now
  {registered, []},
  {env, [
  	{resources, "priv/gx"}
  ]},
  {applications, [kernel, stdlib]}
]}.
