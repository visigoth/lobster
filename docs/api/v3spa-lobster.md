# v3spa-lobster API Reference

James Bielman `<jamesjb@galois.com>`
Jesse Hallett `<jesse@galois.com>`

# Revision History

## Revision 1 (16 November 2015):

- Document v3spa-lobster 1.0 interface.

## Revision 2 (9 December 2015):

- Fix bugs, document data structures in TypeScript.

## Revision 3 (20 April 2016):

- Document v3spa-lobster 2.0 interface.

# Introduction

The V3SPA IDE and the Lobster DSL communicate via
a web service that provides a JSON-based API to import, export,
and analyze Lobster policies. This document describes the data
types and request URLs that make up this Lobster API.

# Lobster Utilities

The Lobster distribution also contains several command-line
utilities that can be used to test these features outside of
the IDE. In this section, we document the most useful utilities
that can be used to exercise the Lobster DSL.

## m4-lobster

The `m4-lobster` utility imports SELinux policy projects using M4
macros into the Lobster language. It is equivalent to the
`/projects/:name/import/selinux` endpoint in the V3SPA REST API.

Running this command will import a reference policy from the
`v3spa-server/data/refpolicy/20130424` directory, writing the
output to a single Lobster module in `refpolicy.lsr`.

```sh
$ m4-lobster v3spa-server/data/refpolicy/20130424 > refpolicy.lsr
```

## lobster-json

The `lobster-json` utility evaluates a Lobster module and converts it
to the JSON representation used by the IDE for visualization.

Running this command will convert `refpolicy.lsr` to `refpolicy.json`:

```sh
$ lobster-json refpolicy.lsr > refpolicy.json
```

## v3spa-server

The `v3spa-server` application runs the Web API server for the Lobster
integration with the Invincea IDE. To start the web service, simply
run the `v3spa-server` program from a directory that contains a `refpolicy`
subdirectory, which is used by the `/import/selinux` endpoint.

The server will create a directory called `projects` under the working
directory. Policy files that are uploaded or imported to the server will be
stored in the `projects` directory.

# V3SPA 2.0 API Endpoints

Path                                       Method    Request Format                       Result Type
----                                       ------    --------------                       ---------------
`/version`                                 `GET`     -                                    `null`
`/import/iptables`                         `POST`   `text/x-iptables`                     `application/vnd.lobster`
`/projects`                                `GET`    -                                     `application/vnd.v3spa.projects+json`
`/projects/:name`                          `POST`   -                                     `application/vnd.v3spa.project+json`
`/projects/:name`                          `GET`    -                                     `application/vnd.v3spa.project+json`
`/projects/:name`                          `DELETE` -                                     `null`
`/projects/:name/import/selinux`           `POST`   `application/vnd.v3spa.selinux+json`  `null`
`/projects/:name/modules`                  `POST`   `multipart/form-data`                 `application/vnd.v3spa.project+json`
`/projects/:name/modules/:module`          `GET`    -                                     `application/vnd.lobster`
`/projects/:name/modules/:module`          `DELETE` -                                     `null`
`/projects/:name/modules/:module/selinux`  `GET`    -                                     `text/x-type-enforcement`
`/projects/:name/paths`                    `GET`    -                                     `application/vnd.v3spa.pathset+json`

With the exception of the `/projects/:name/modules/:module` endpoints,
responses will actually be wrapped in a response of type
`application/vnd.v3spa.result+json`, which includes a `result` and an `errors`
field. The result type described in the table above will appear under
a `result` field in the top-level wrapper.

Media Type                            Description
----------                            -----------
`application/vnd.lobster`              Lobster source file
`application/vnd.v3spa.pathset+json`   description of information flow in a set of modules, in JSON format
`application/vnd.v3spa.project+json`   project metadata, including a list of files associated with the project
`application/vnd.v3spa.projects+json`  list of projects
`application/vnd.v3spa.result+json`    result JSON type; lists any errors, and reports server API version
`application/vnd.v3spa.selinux+json`   JSON representation of SELinux policy
`text/x-iptables`                      raw IPTables policy file
`text/x-type-enforcement`              raw SELinux policy formatted as a `.te` file

For more details on these media types, see the API Types section below.

In V3SPA 2.0, we propose a REST interface that stores Lobster
module state on the server to avoid repeatedly sending the source
to the server on each query.

A project is either created with an existing Lobster policy or imported
from SELinux or IPTables, returning a project identifier. This identifier
can then be passed back to perform queries such as returning JSON for
visualization.

## Basic Workflow

Policy files are uploaded to the server, and are stored under a project
namespace. Files are stored in Lobster format. SELinux policies can be uploaded
(in JSON format); these are converted to Lobster files for storage on the server.

Lobster files are uploaded to the server in batches using the
`/projects/:name/modules` endpoint. Files are provided using
`multipart/form-data` format.

```sh
$ curl -X POST \
    -F 'file=@moduleA.lsr;filename=moduleA.lsr' \
    -F 'file=@moduleB.lsr;filename=moduleB.lsr' \
    http://localhost:8000/projects/myproject/modules
```

SELinux policies are uploaded to the `projects/:name/import/selinux` endpoint:

```sh
$ curl -H 'Content-Type: application/vnd.v3spa.selinux+json' -X POST \
    --data-binary @policy.json \
    http://localhost:8000/projects/myproject/import/selinux
```

Once the server has some stored policies, those polices can be queried.
To get a list of information-flow paths through all files in a project, use the
`/projects/:name/paths` endpoint:

```sh
$ curl 'http://localhost:8000/projects/myproject/paths?id=1'
```

See the documentation below for detailed information on each of these endpoints.

## `GET /version`

Returns the version of the Lobster server. This simply returns a
`Result` with a `null` value in the `result` field. The server API version will
be included in the `version` field, as is the case with all JSON responses.

## `POST /import/iptables`

In the future there will be an endpoint that imports IPTables policies, and
stores the resulting Lobster files in a project namespace. In the meantime this
endpoint has been preserved from the v1 API.

Convert IPTables policy to Lobster. The `POST` data is a IPTables policy
source, and the result upon success is raw Lobster policy module.

## `GET /projects`

Responds with a list of all projects stored on the server.

## `POST /projects/:name`

Creates an empty project with the given name if none exists, and responds with
project metadata. Responds with metadata of existing project otherwise.

## `GET /projects/:name`

Responds with project metadata in `application/vnd.v3spa.project+json` format.
This includes a list of all stored project files.

Responds with `404` status if there is no project with the given name.

## `DELETE /projects/:name`

Delete a project and all of the stored files associated with that project.

## `POST /projects/:name/import/selinux`

Convert an SELinux policy module to Lobster, and store the result in the given
project namespace.

The `POST` body is an SELinux in JSON format.

In the future this endpoint will create a separate Lobster file for each module
in the SELinux policy. For the time being, this endpoint will put everything
into a single file called 'imported/selinux'. (Note that the slash must be
encoded as '%2F' when accessing that file by name). The response includes
a `Location` header with a path to the module that is created.

If there is an existing file called 'imported/selinux' in the given project
namespace, it will be overwritten.

If the named project does not exist, it will be created automatically.

## `POST /projects/:name/modules`

Upload Lobster files in batch. The request format is `multipart/form-data`.

Any files in the uploaded form data will be saved under the given project
namespace. Files will be stored using the `filename` parameter given with the
file in the `multipart/form-data` payload. Therefore a `filename` parameter
*must* be included with each uploaded file.

Existing files with matching names will be overwritten. Existing files with
names that do not conflict with uploaded filenames will not be affected.

Filenames may include slashes. But note that slashes must be URL-encoded as
'%2F' when looking up the file by name using, e.g., the
`/projects/:name/modules/:module` endpoint.

If the named project does not exist, it will be created automatically.

## `GET /projects/:name/modules/:module`

Download a Lobster file that has been stored on the server. The `:module`
parameter is the name of the stored file. The response is raw Lobster format.

Filenames may includes slashes. But note that slashes must be URL-encoded as
'%2F'. The best practice is to use a URL-encode function to encode the entire
project name and filename regardless.

## `DELETE /projects/:name/modules/:module`

Remove a stored file from the server.

## `GET /projects/:name/modules/:module/selinux`

Convert a stored file to a raw SELinux `.te` file, and download the result.

## `GET /projects/:name/paths`

Analyze paths in the information flow graph made up of all stored files under
the given project namespace.

Returns a `application/vnd.v3spa.pathset+json` result.

This endpoint accepts the following query parameters:

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

## `application/vnd.v3spa.selinux+json`

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

## `application/vnd.v3spa.pathset+json`

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
