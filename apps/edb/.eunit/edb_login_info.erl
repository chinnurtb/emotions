-module(edb_login_info).

%-record(login_info, {
		%email :: string(),
		%hash :: string(),
		%salt :: string()
	%}).


%new(Email, Password) ->
	%Salt = edb_id:new(),
	%#login_info{
		%email = Email,
		%hash = salt_and_hash_password(Password, Salt),
		%salt = Salt
	%}.

%salt_and_hash_password(Password, Salt) ->
	%Data = <<Salt, "sicret", Password>>,
	%<<Number:160/integer>> = crypto:sha(Data),
	%integer_to_list(Number, 16).
