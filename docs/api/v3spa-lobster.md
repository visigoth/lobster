# v3spa-lobster API Reference

James Bielman `<jamesjb@galois.com>`

# Revision History

## Revision 1 (16 November 2015):

- Document v3spa-lobster 1.0 interface.

## Revision 2 (9 December 2015):

- Fix bugs, document data structures in TypeScript.

# Introduction

The V3SPA IDE and the Lobster DSL communicate via
a web service that provides a JSON-based API to import, export,
and analyze Lobster policies. This document describes the data
types and request URLs that make up this Lobster API.

# V3SPA 1.0 API Endpoints

Path                          Method    Request Format  Result Type
----                          ------    --------------  ---------------
`/version`                    `GET`     -               `null`
`/parse`                      `POST`    `raw`           `Module`
`/paths`                      `POST`    `raw`           `Paths`
`/import/iptables`            `POST`    `raw`           `string`
`/import/selinux`             `POST`    `SEPolicy`      `string`
`/export/selinux`             `POST`    `raw`           `string`

Endpoints with a 'Request Format' of `raw` receive their input
by `POST`ing policy source directly. All other formats are JSON
requests defined in this document.

All endpoints return a `Result` JSON response, with the `result`
field containing an object of the type described in 'Result Type'.

# V3SPA 2.0 Proposed API Endpoints

Path                          Method    Request Format  Result Type
----                          ------    --------------  ---------------
`/project/new`                `POST`    TBD             TBD
`/project/import/selinux`     `POST`    TBD             TBD
`/project/import/iptables`    `POST`    TBD             TBD
`/project/export/selinux/:id` `POST`    TBD             TBD
`/project/source/:id`         `GET`     TBD             TBD
`/project/source/:id`         `PUT`     TBD             TBD
`/project/json/:id`           `GET`     TBD             TBD
`/project/paths`              `POST`    TBD             TBD

In V3SPA 2.0, we propose a REST interface that stores Lobster
module state on the server to avoid repeatedly sending the source
to the server on each query.

A project is either created with an existing Lobster policy or imported
from SELinux or IPTables, returning a project identifier. This identifier
can then be passed back to perform queries such as returning JSON for
visualization.

## `GET /version`

Returns the version of the Lobster server. This simply returns a
`Result` with a `null` value in the `result` field.

## `POST /parse`

Parse a complete Lobster policy into a JSON representation for
visualization. The `POST` body must contain a complete Lobster policy
definition.

Returns a result of type `Module`.

### Parameters

The following query parameters may be included to filter the results:

`maxdepth`

  ~ : integer

      The maximum depth to expand nodes, by default.

`path`

  ~ : string

      Paths to expand children of, regardless of `maxdepth`. The path
      must be included in the original result set to be eligible for
      expansion.

`id`

  ~ : integer

      Node identifiers to expand to children of, regardless of `maxdepth`. The
      node with this ID must be included in the original result set to be
      eligible for expansion.

### Examples

`POST /parse?maxdepth=1&path=apache&path=ssh`

Return JSON showing only top-level domains, except for the `apache`
and `ssh` modules, which will be expanded to show their immediate children.

## `POST /paths`

Analyze paths in the information flow graph.

The `POST` body is a complete Lobster module in source form.

Returns a `PathSet` result.

### Parameters

`id`
  ~ : `number` **required**

    Domain ID to begin path analysis from.

`direction`
  ~ : `string`

    Information flow direction to analyze:

    - `forward` - find domains that can be accessed by domain
    - `backward` - find domains that can access domain

`limit`
  ~ : `number`

    Limit the maximum number of results.

`perms`
  ~ : `string`

    Comma-separated list of permissions to traverse on connections
    to or from the initial domain.

`trans_perms`
  ~ : `string`

    Comma-separated list of permissions to traverse on connections
    between domains other than the initial domain.

## `POST /import/iptables`

Convert IPTables policy to Lobster.

The `POST` data is a IPTables policy source, and the result upon success
is raw Lobster policy module.

## `POST /import/selinux`

Convert an SELinux policy module to Lobster.

The `POST` body is an `SEPolicy` in JSON format.

The result upon success is a raw Lobster policy module.

## `POST /export/selinux`

Export a Lobster policy as an SELinux policy.

The `POST` data is raw Lobster policy source, and the result upon
success is a raw SELinux `.te` file.

# API Types

Definitions of JSON types are shown in TypeScript syntax for reference.

## `Result`

All responses from the server are returned as a JSON object containing
the server's version, a list of errors, and a result.

~~~~javascript
interface Result<T> {
  version: number;
  errors: Error[];
  result: T;
}
~~~~

### Fields

`version`

  ~ : `number`

    Protocol version of the server.

`errors`

  ~ : `[Error]`

    List of `Error` objects if the request was not successful.

`result`

  ~ : `object`

    Result of the request if successful. The type of object depends
    on the API call---see the *API Endpoints* table.

### Examples

A response with no errors and an integer result:

~~~~json
{
  "version": 5,
  "errors": [],
  "result": 100
}
~~~~

## `Error`

An error returned from the V3SPA Lobster source.

~~~~javascript
interface Error {
  filename: string;
  message: string;
  srcloc: SourceSpan;
}
~~~~

### Fields

`filename`
  ~ : `string`

    Source file containing the error, if known.

`message`
  ~ : `string`

    Text of the error message to display to the user.

`srcloc`
  ~ : `SourceSpan`

    The source location of the error.

### Examples

A syntax error on the first line of `test.lsr`:

~~~~json
{
  "filename": "test.lsr",
  "message": "Syntax Error",
  "srcloc": {
    "start": {
      "line": 1,
      "col": 0,
    },
    "end": {
      "line": 2,
      "col": 0,
    }
  }
}
~~~~

## `SourceSpan`

A source span within a file.

~~~~javascript
interface SourceSpan {
  start: SourcePos;
  end: SourcePos;
}
~~~~

### Fields

`start`
  ~ : `SourcePos`

    Starting location of the source span.

`end`
  ~ : `SourcePos`

    Ending location of the source span.

### Examples

The first line of a file:

~~~~json
{
  "start": {
    "line": 1,
    "col": 0
  },
  "end": {
    "line": 2,
    "col": 0    
  }
}
~~~~

## `SourcePos`

A position within a source file.

~~~~javascript
interface SourcePos {
  line: number;
  col: number;
}
~~~~

### Fields

`line`
  ~ : `number`

    Line number of the position.

`col`
  ~ : `number`

    Column number of the position.

### Examples

The first character of the second line in a source file:

~~~~json
{
  "line": 2,
  "col": 0
}
~~~~

## `Module`

List of all objects in a Lobster module. Each domain, port, and
connection has a unique identifier.

~~~~javascript
interface Module {
  domains: Map<string, Domain>;
  ports: Map<string, Port>;
  connections: Map<string, Connection>;
  root: string;
}
~~~~

### Fields

`domains`
  ~ : `Map<string, Domain>`

    Map of domain IDs to `Domain` objects.

`ports`
  ~ : `Map<string, Port>`

    Map of port IDs to `Port` objects.

`connections`
  ~ : `Map<string, Connection>`

    Map of connection IDs to `Connection` objects.

`root`
  ~ : `string`

    ID of the root domain.

## `Domain`

JSON representation of a Lobster domain.

In Lobster modules generated from SELinux policy, a domain represents
an SELinux `type`, `attribute`, or `type_transition`. Annotations on
the domain can be used to distinguish between these when necessary.

~~~~javascript
interface Domain {
  name: string;
  path: string;
  class: string;
  subdomains: Map<string, Subdomain>;
  parent: string;
  ports: string[];
  classAnnotations: Annotation[];
  domainAnnotations: Annotation[];
  srcloc: SourceSpan;
}
~~~~

### Fields

`name`
  ~ : `string`

    Name of this domain.

`path`
  ~ : `string`

    Fully-qualified path of this domain including names of
    containing domains.

`class`
  ~ : `string`

    Name of the class this domain was instantiated from.

`subdomains`
  ~ : `Map<string, Subdomain>`

    Map containing the domain IDs and names of this domain's children.
    The `Subdomain` values contain the name and are useful for showing
    links to subdomains that are filtered out. These domain IDs can
    be referenced in the `domains` map inside the containing `Module`.

`parent`
  ~ : `string`

    ID of the domain that contains this domain. This is `null` for the
    root domain in the module.

`ports`
  ~ : `string[]`

    List of port IDs contained in this domain. These IDs can be
    referenced in the `ports` map inside the containing `Module`.

`classAnnotations`
  ~ : `Annotation[]`

    List of annotations on the domain's class definition.

`domainAnnotations`
  ~ : `Annotation[]`

    List of annotations on the domain's definition.

`srcloc`
  ~ : `SourceSpan`

    Source location of the definition of this domain, if known.

## `Subdomain`

Summary information about a subdomain. This is used to have
access to the names of subdomains that may be filtered out.

~~~~javascript
interface Subdomain {
  name: string;
}
~~~~

### Fields

`name`
  ~ : `string`

    Name of the subdomain.

## `Port`

Information about a port associated with a domain.

In policy imported from SELinux, a port represents an endpoint
that may be used to create an `allow` rule between two types or
attributes. Ports are named after the permission classes on SELinux
types, such as `file` or `socket`. The special port `active` is
used when a domain is the `subject` in an allow rule---in other
words, when the domain is an entity capable of performing actions
on objects, such as a process.

~~~~javascript
interface Port {
  name: string;
  path: string;
  annotations: Annotation[];
  position: string;
  srcloc: SourceSpan;
  domain: string;
}
~~~~

### Fields

`name`
  ~ : `string`

    Name of the port.

`path`
  ~ : `string`

    Fully-qualified name of the port including containing domains.

`annotations`
  ~ : `Annotation[]`

    List of annotations on the port definition.

`position`
  ~ : `string`

    Position of this port, either `'subject'`, `'object'`, or `null`.

`srcloc`
  ~ : `SourceSpan`

    Source location of the port's definition.

`domain`
  ~ : `string`

    ID of the domain that contains this port.

### Examples

Given this Lobster module:

~~~~
class App {
  port read : {position = object};
}

domain app = App();
~~~~

The corresponding JSON for the port `app.read` is:

~~~~json
{
  "annotations": [],
  "srcloc": {
    "start": {
      "line": 11,
      "col": 3
    },
    "end": {
      "line": 11,
      "col": 35
    }
  },
  "path": "app.read",
  "domain": "1",
  "name": "read",
  "position": "object"
}
~~~~

## `Connection`

Connections are links between two ports.

In policies generated from SELinux, a connection often represents
an `allow` rule between a `subject` and an `object` type.

~~~~javascript
interface Connection {
  left: string;
  left_dom: string;
  right: string;
  right_dom: string;
  level: string;
  connection: string;
  annotations: Annotation[];
  srcloc: SourceSpan;
}
~~~~

### Fields

`left`
  ~ : `string`

    ID of the port on the left hand side of the connection.

`left_dom`
  ~ : `string`

    ID of the domain containing the port on the left hand side.

`right`
  ~ : `string`

    ID of the port on the right hand side of the connection.

`right_dom`
  ~ : `string`

    ID of the domain containing the port on the right hand side.

`level`
  ~ : `string`

    Field describing whether the connection is between peers, internal,
    or between a parent and child.

    Legal values are:

    - `peer` for a connection between two domains with the same parent.
    - `parent` for a connection from a parent on the left to child on the right.
    - `child` for a connection from a child on the left to a parent on the right.
    - `internal` for a connection between two ports inside the same domain.

`connection`
  ~ : `string`

    Field describing the direction of the connection. This is currently
    not used in policy generated from SELinux---all connections are `neutral`.

    Legal values are:

    - `left-to-right` for connections defined with `-->`
    - `right-to-left` for connections defined with `<--`
    - `bidirectional` for connections defined with `<-->`
    - `neutral` for connections defined with `--`
    - `negative` for connections defined with `-/-`

`annotations`
  ~ : `Annotation[]`

    List of annotations on the connection statement.

`srcloc`
  ~ : `SourceSpan`

    Source location of the connection statement.

### Examples

Given this Lobster definition:

~~~~
[Perm("file", "read")]
httpd_t.active -- httpd_file_t.file;
~~~~

the JSON representation is:

~~~~json
{
  "left": "0",
  "left_dom": "0",
  "right": "1",
  "right_dom": "1",
  "level": "peer",
  "connection": "neutral",
  "annotations": [
    {
      "name": "Perm",
      "args": ["file", "read"]
    }
  ],
  "srcloc": null
}
~~~~

## `Annotation`

Annotations are meta-information added to Lobster statements used
to include data that is not part of the information flow graph.

For example, when SELinux policy is imported into Lobster, domains
are created for both `type` and `attribute` statements. An annotation
is attached to the domain to track which statement was used in the
original policy so that it can be exported back to SELinux syntax.

~~~~javascript
interface Annotation {
  name: string;
  args: any[];
}
~~~~

### Fields

`name`
  ~ : `string`

    Name of the annotation.

`args`
  ~ : `any[]`

    List of argument literals to the annotation declaration.

### Examples

The following annotation:

~~~~
[Test("hello", 1, 2)]
~~~~

is represented in JSON as follows:

~~~~json
{
  "name": "Test",
  "args": ["hello", 1, 2]
}
~~~~

## `SEPolicy`

A request to import one or more SELinux policy modules against
a reference policy.

~~~~javascript
interface SEPolicy {
  refpolicy: string;
  modules: ModuleSource[];
}
~~~~

### Fields

`refpolicy`
  ~ : `string`

    Name of the reference policy to use as a base. This is
    used to construct a path in the server's `data` directory
    to locate the base policy.

`modules`
  ~ : `ModuleSource[]`

    List of modules to import, replacing any modules with the
    same name in the base reference policy.

## `ModuleSource`

A single SELinux module to import.

~~~~javascript
interface ModuleSource {
  name: string;
  if: string;
  te: string;
  fc: string;
}
~~~~

### Fields

`name`
  ~ : `string`

    Name of this module, with no file extension.

`if`
  ~ : `string`

    Contents of the module's `.if` file.

`te`
  ~ : `string`

    Contents of the module's `.te` file.

`fc`
  ~ : `string`

    Contents of the module's `.fc` file.

## `PathSet`

A set of domains reachable from an initial domain, as returned by
a path query.

~~~~javascript
interface PathSet {
  truncated: boolean;
  [index: string]: PathNode[];
}
~~~~

### Fields

`truncated`
  ~ : `boolean`

    True if a `limit` was supplied to the path query and paths were
    not included in the results.

*DOMAIN_ID*
  ~ : `PathNode[]`

      A domain ID reachable from the initial domain along with the
      path to it.

## `PathNode`

A path node from one domain to another via a connection.

~~~~javascript
interface PathNode {
  conn: string;
  left: string;
  right: string;
}
~~~~

### Fields

`conn`
  ~ : `string`

    ID of the connection that was traversed.

`left`
  ~ : `string`

    ID of the left hand side domain of the connection.

`right`
  ~ : `string`

    ID of the right hand side domain of the connection.
