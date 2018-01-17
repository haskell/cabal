-- | This module contains a SPDX data from specification version 2.1
--
-- Specification is available on <https://spdx.org/specifications>
module Distribution.SPDX (
    -- * License
    License (..),
    -- * License expression
    LicenseExpression (..),
    simpleLicenseExpression,
    OnlyOrAnyLater (..),
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
    unsafeMkLicenseRef,
    ) where

import Distribution.SPDX.LicenseExceptionId
import Distribution.SPDX.License
import Distribution.SPDX.LicenseId
import Distribution.SPDX.LicenseExpression
import Distribution.SPDX.LicenseReference
