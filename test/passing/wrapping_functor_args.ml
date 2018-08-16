(* This declaration looks odd *)
type request_token =
  Sociaml_oauth_client.Client.Make(Sociaml_oauth_client.Posix.Clock)
    (Sociaml_oauth_client.Posix.MAC_SHA1)
    (Sociaml_oauth_client.Posix.Random)
  .request_token

(* Whereas this one works well *)
module OauthClient =
  Sociaml_oauth_client.Client.Make
    (Sociaml_oauth_client.Posix.Clock)
    (Sociaml_oauth_client.Posix.MAC_SHA1)
    (Sociaml_oauth_client.Posix.Random)
