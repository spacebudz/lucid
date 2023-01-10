import LumeCode from "./scripts/components/lume_code.js";
import LumeCarousel from "./scripts/vendor/carousel/carousel.js";
import LumeCarouselControls from "./scripts/components/lume_carousel_controls.js";
import LumeFilter from "./scripts/components/lume_filter.js";

customElements.define("lume-code", LumeCode);
customElements.define("lume-carousel", LumeCarousel);
customElements.define("lume-carousel-controls", LumeCarouselControls);
customElements.define("lume-filter", LumeFilter);

// For testing purpose of CSP middleware
const userAgentString = navigator.userAgent;
const chromeAgent = userAgentString.indexOf("Chrome") > -1;

if (chromeAgent) {
  const observer = new ReportingObserver((reports) => {
    for (const report of reports) {
      console.log(report.type, report.url, report.body);
    }
  }, { buffered: true });

  observer.observe();
}
