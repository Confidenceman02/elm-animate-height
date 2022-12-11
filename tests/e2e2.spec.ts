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

  describe("Transition example", () => {
    it("Does not show the content by default", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/Transition.elm`);

      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeFalsy();
    });

    it("displays content when animation ends", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/Transition.elm`);

      await page.click("button:has-text('Toggle')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeTruthy();
    });

    it("Hides the content when togling after content shows", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/Transition.elm`);

      await page.click("button:has-text('Toggle')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      await page.waitForTimeout(400);
      await page.click("button:has-text('Toggle')");
      await page.waitForTimeout(400);
      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeFalsy();
    });

    it("displays content when animation ends with Fix", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/Transition.elm`);

      await page.click("button:has-text('Fix')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeTruthy();
    });
  });
});
