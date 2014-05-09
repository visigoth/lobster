--
-- Version.hs --- Versioning for the V3SPA web service.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module V3SPA.Server.Version where

-- | Version number of the V3SPA server and protocol.
--
-- Changelog:
--
-- Version 0:
--
-- - initial release before version number
--
-- Version 1:
--
-- - added annotation support for Lobster
-- - added "/version" to query server version
-- - JSON changes:
--   - added "annotations" field to connection objects
--   - added "domainAnnotations" and "classAnnotations" to domain
--   - added "version" at top level
--   - implemented source positions in error messages
--
-- Version 2:
--
-- - changed Lobster JSON format to flat list of domains,
--   ports, and connections
--
-- Version 3:
--
-- - added filter query parameters to "/parse".
--
-- Version 4:
--
-- - subdomains are now a map of keys to objects with their name
--
-- Version 5:
--
-- - connections are now a map indexed by connection ID
-- - added '/paths?id=N' for path queries starting at domain N
-- - added 'left_dom' and 'right_dom' to connections
version :: Int
version = 5

