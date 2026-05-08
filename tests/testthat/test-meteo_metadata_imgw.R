context("meteo-metadata")

test_that("meteo_metadata_imgw tests", {
  #skip("meteo-metadata skipping")  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    m_hs = meteo_metadata_imgw("hourly", "synop")
    m_hc = meteo_metadata_imgw("hourly", "climate")
    m_ds = meteo_metadata_imgw("daily", "synop")
    m_dc = meteo_metadata_imgw("daily", "climate")
    m_dp = meteo_metadata_imgw("daily", "precip")
    m_ms = meteo_metadata_imgw("monthly", "synop")
    m_mc = meteo_metadata_imgw("monthly", "climate")
    m_mp = meteo_metadata_imgw("monthly", "precip")
  
    expect_error(meteo_metadata_imgw("hourly", "precip"))
    
    if (is.list(m_hs) && is.list(m_ds) && is.list(m_ds) && is.list(m_dc) && 
        is.list(m_dp) && is.list(m_ms) && is.list(m_mc) && is.list(m_mp)) {
      expect_equal(dim(m_hs[[1]]), c(107, 2))
      expect_equal(dim(m_hc[[1]]), c(22, 2))
      expect_equal(dim(m_ds[[1]]), c(65, 2))
      expect_equal(dim(m_ds[[2]]), c(23, 2))
      expect_equal(dim(m_dc[[1]]), c(18, 2))
      expect_equal(dim(m_dc[[2]]), c(13, 2))
      expect_equal(dim(m_dp[[1]]), c(16, 2))
      expect_equal(dim(m_ms[[1]]), c(58, 2))
      expect_equal(dim(m_ms[[2]]), c(22, 2))
      expect_equal(dim(m_mc[[1]]), c(27, 2))
      expect_equal(dim(m_mc[[2]]), c(12, 2))
      expect_equal(dim(m_mp[[1]]), c(14, 2))
    }
  }
})
