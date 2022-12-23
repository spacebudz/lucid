export default class LumeCode extends HTMLElement {
  connectedCallback() {
    const buttons = this.querySelectorAll("button");
    const carousel = document.querySelector(this.getAttribute("target"));

    buttons.forEach((button) => {
      button.addEventListener("click", () => {
        if (button.value === "next") {
          carousel.next();
        } else {
          carousel.prev();
        }
      });
    });

    //Ocultar botóns ao inicio/fin
    let isScrolling;

    carousel.addEventListener(
      "scroll",
      () => {
        clearTimeout(isScrolling);
        isScrolling = setTimeout(showHideButtons, 50);
      },
      false,
    );

    carousel.addEventListener("mouseenter", showHideButtons);

    function showHideButtons() {
      buttons.forEach((button) => {
        switch (button.value) {
          case "next":
            button.disabled = carousel.scrollFromRight < 5;
            break;
          case "prev":
            button.disabled = carousel.scrollFromLeft < 5;
            break;
        }
      });
    }
    showHideButtons();
  }
}
