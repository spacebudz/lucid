// ARIA: tab role, best practices: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/tab_role
// customElements eventListener: https://www.jonasfaehrmann.com/writing/post/2021-02-01-custom-elements-event-listener-and-this/

export default class LumeCode extends HTMLElement {
  constructor() {
    super();
    this.tabFocus = 0;
    this.tabs = this.querySelectorAll('[role="tab"]');
    this.tabList = this.querySelector('[role="tablist"]');
    this.buttonBoundListener = this.handleTabChange.bind(this);
    this.keydownBoundListener = this.handleKeyPress.bind(this);
  }

  connectedCallback() {
    this.tabs.forEach((tab) => {
      tab.addEventListener("click", this.buttonBoundListener);
    });

    this.tabList.addEventListener("keydown", this.keydownBoundListener);
  }

  handleKeyPress(e) {
    if (e.keyCode === 39 || e.keyCode === 37) {
      this.tabs[this.tabFocus].setAttribute("tabindex", -1);
      if (e.keyCode === 39) {
        this.tabFocus++;
        if (this.tabFocus >= this.tabs.length) {
          this.tabFocus = 0;
        }
      } else if (e.keyCode === 37) {
        this.tabFocus--;
        if (this.tabFocus < 0) {
          this.tabFocus = this.tabs.length - 1;
        }
      }

      this.tabs[this.tabFocus].setAttribute("tabindex", 0);
      this.tabs[this.tabFocus].focus();
    }
  }

  handleTabChange(e) {
    const target = e.target;
    const parent = target.parentNode;
    const grandparent = parent.parentNode;

    const current = target.getAttribute("aria-controls");

    grandparent.querySelectorAll('[aria-selected="true"]').forEach((t) => {
      if (t === target) return;
      t.setAttribute("aria-selected", false);
      t.setAttribute("tabindex", -1);
      t.classList.remove("is-active");
    });

    target.setAttribute("aria-selected", true);
    target.setAttribute("tabindex", 0);
    target.classList.add("is-active");

    grandparent.parentNode.querySelectorAll('[role="tabpanel"]').forEach(
      (p) => {
        if (p.id === current) {
          p.removeAttribute("hidden");
        } else {
          p.setAttribute("hidden", true);
        }
      },
    );
  }
}
