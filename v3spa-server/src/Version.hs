--
-- Version.hs --- Versioning for the V3SPA web service.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Version where

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
version :: Int
version = 1

