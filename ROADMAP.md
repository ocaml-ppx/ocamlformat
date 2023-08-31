# Roadmap

Code formatting should be completely transparent for users. All the while, it should be customizable so users can define their own coding style if they want to.

ocamlformat currently provides a large set of options and we will strive to support new options that the community cares about. However, we recognize that there's still a long road ahead to make the formatting experience seamless and well-integrated into the other development workflows.

Based on these design principles for ocamlformat and the user feedback we've received, our roadmap to ocamlformat 1.0 includes:

- **Versioning:** Ocamlformat 1.1 should be backward compatible and allow users to format code as they did with 1.0 if the version is specified in the configuration file.
- **Customization:** Continue to support and add formatting options ocamlformat users care about.
- **Concrete Syntax:** We'll migrate ocamlformat from an AST to a CST that contains more information about the syntax used such as `begin end`, syntax sugars, and more.
- **Unopiniated default:** We'd like to move to a default profile that is based on objective heuristics. We will release a `diff-friendly` profile and iterate on it to make it the `default` profile if community feedback is positive.
- **Dune integration:** Improve the integration in Dune to remove the need for a configuration file and allow users to configure formatting in their `dune-project`.
