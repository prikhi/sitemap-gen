# sitemap-gen

[![sitemap-gen Build Status](https://github.com/prikhi/sitemap-gen/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/sitemap-gen/actions/workflows/main.yml)


`sitemap-gen` is a Haskell library for generating XML sitemaps and sitemap
index files.

It uses the [xmlgen][xmlgen] library to generate XML that conforms to the
[sitemaps.org][sitemap-schema] XML schema.

To use this library, build a `Sitemap` or `SitemapIndex` type and use the
respective `render...` functions to build the `ByteString` output:

```haskell
import Web.Sitemap.Gen (Sitemap(..), SitemapUrl(..), renderSitemap)

import qualified Data.ByteString as BS
import qualified Web.Sitemap.Gen as Sitemap

mySitemap :: BS.ByteString
mySitemap =
    let urls =
            [ SitemapUrl
                { sitemapLocation = "https://mydomain.com/my/url/"
                , sitemapLastModified = Nothing
                , sitemapChangeFrequency = Just Sitemap.Monthly
                , sitemapPriority = Just 0.9
                }
            , SitemapUrl
                { sitemapLocation = "https://mydomain.com/lower/priority/"
                , sitemapLastModified = Nothing
                , sitemapChangeFrequency = Just Sitemap.Yearly
                , sitemapPriority = Just 0.4
                }
            ]
    in
    renderSitemap $ Sitemap urls
```


## Develop

Build the package, documentation, & tests with `stack`:

```sh
stack build --pedantic --haddock --test --file-watch
```


## License

BSD-3, exceptions possible.


[xmlgen]: https://hackage.haskell.org/package/xmlgen
[sitemap-schema]: https://www.sitemaps.org/protocol.html#index
