export default class Player {
  constructor(carousel) {
    this.carousel = carousel;
    this.interval = 5000;
    this.step = 1;
    this.isPlaying = false;
  }

  play(interval = this.interval) {
    const go = () => {
      if (!this.carousel.scrollFromLeft) {
        this.step = 1;
      } else if (!this.carousel.scrollFromRight) {
        this.step = -1;
      }

      this.carousel.index += this.step;
      this.play();
    };

    this.isPlaying = true;
    this.timeout = setTimeout(go, interval);
  }

  stop() {
    clearInterval(this.timeout);
    this.isPlaying = false;
  }
}
