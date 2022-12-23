export default class LumeFilter extends HTMLElement {
  connectedCallback() {
    const form = this.querySelector("form");
    const items = this.querySelectorAll(":scope > ul > li");

    const init = new URLSearchParams(window.location.search);

    for (const key of init.keys()) {
      const input = form[key];

      if (input) {
        input.checked = true;
      }
    }

    form.addEventListener("submit", (event) => {
      onChange();
      event.preventDefault();
    });
    form.addEventListener("input", onChange);

    onChange();

    function onChange() {
      form.querySelectorAll("input[type='checkbox']").forEach((input) => {
        const btn = input.closest(".button");

        if (btn) {
          btn.classList.toggle("is-active", input.checked);
        }
      });

      const data = new FormData(form);
      filter(data);
      const permalink = new URLSearchParams(data).toString();

      if (permalink !== document.location.search) {
        const newUrl = permalink ? `?${permalink}` : document.location.pathname;
        history.pushState({}, null, newUrl);
      }
    }

    function filter(data) {
      const tags = [];
      let status;

      for (const [name, value] of data.entries()) {
        if (name === "status") {
          status = value;
          continue;
        }

        tags.push(name);
      }

      items.forEach((item) => {
        switch (status) {
          case "enabled":
            if (!item.classList.contains("is-enabled")) {
              item.hidden = true;
              return;
            }
            break;
          case "disabled":
            if (item.classList.contains("is-enabled")) {
              item.hidden = true;
              return;
            }
            break;
        }

        item.hidden = tags.length &&
          tags.every((tag) => !item.dataset.tags.split(",").includes(tag));
      });
    }
  }
}
