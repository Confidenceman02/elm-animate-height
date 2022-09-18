import { chromium, Browser } from "playwright";
import { expect } from "@playwright/test";
let browser: Browser;
const BASE_URI = "http://localhost:8000";

before(async () => {
  browser = await chromium.launch();
});

after(async () => {
  await browser.close();
});

describe("examples", () => {
  it("has all examples", async () => {
    const page = await browser.newPage();
    await page.goto(BASE_URI);

    const transitionExampleVisible = await page.isVisible(
      "text=Transition.elm"
    );

    expect(transitionExampleVisible).toBeTruthy();
  });
});
