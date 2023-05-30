-- | This module implements SPDX specification version 2.1 with a version 3.0 license list.
--
-- Specification is available on <https://spdx.org/specifications>
module Distribution.SPDX
  ( -- * License
    License (..)

    -- * License expression
  , LicenseExpression (..)
  , SimpleLicenseExpression (..)
  , simpleLicenseExpression

    -- * License identifier
  , LicenseId (..)
  , licenseId
  , licenseName
  , licenseIsOsiApproved
  , mkLicenseId
  , licenseIdList

    -- * License exception
  , LicenseExceptionId (..)
  , licenseExceptionId
  , licenseExceptionName
  , mkLicenseExceptionId
  , licenseExceptionIdList

    -- * License reference
  , LicenseRef
  , licenseRef
  , licenseDocumentRef
  , mkLicenseRef
  , mkLicenseRef'

    -- * License list version
  , LicenseListVersion (..)
  , cabalSpecVersionToSPDXListVersion
  ) where

import Distribution.SPDX.License
import Distribution.SPDX.LicenseExceptionId
import Distribution.SPDX.LicenseExpression
import Distribution.SPDX.LicenseId
import Distribution.SPDX.LicenseListVersion
import Distribution.SPDX.LicenseReference
