-- | This module implements SPDX specification version 2.1 with a version 3.0 license list.
--
-- Specification is available on <https://spdx.org/specifications>
module Distribution.SPDX (
    -- * License
    License (..),
    -- * License expression
    LicenseExpression (..),
    SimpleLicenseExpression (..),
    simpleLicenseExpression,
    -- * License identifier
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
    -- * License exception
    LicenseExceptionId (..),
    licenseExceptionId,
    licenseExceptionName,
    mkLicenseExceptionId,
    -- * License reference
    LicenseRef,
    licenseRef,
    licenseDocumentRef,
    mkLicenseRef,
    mkLicenseRef',
    ) where

import Distribution.SPDX.LicenseExceptionId
import Distribution.SPDX.License
import Distribution.SPDX.LicenseId
import Distribution.SPDX.LicenseExpression
import Distribution.SPDX.LicenseReference
