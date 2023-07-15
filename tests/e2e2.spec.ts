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
    const viewSwitchExampleVisible = await page.isVisible(
      "text=ViewSwitch.elm"
    );

    expect(transitionExampleVisible).toBeTruthy();
    expect(viewSwitchExampleVisible).toBeTruthy();
  });

  describe("ViewSwitch example", () => {
    it("Does not show the content by default", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);
      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeFalsy();
    });

    it("Displays content", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 1')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeTruthy();
    });

    it("Displays content for View 1", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 1')");
      await page.waitForSelector("[data-test-id=animate-height-content]");

      await expect(page.getByText("Text for view 1")).toBeVisible();
    });

    it("Displays content for View 2", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 2')");
      await page.waitForSelector("[data-test-id=animate-height-content]");

      await expect(page.getByText("Text for view 2")).toBeVisible();
    });

    it("Displays then closes content for View 1", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 1')");
      await page.waitForSelector("[data-test-id=animate-height-content]");

      await expect(page.getByText("Text for view 1")).toBeVisible();

      await page.click("button:has-text('Close')");
      await page
        .locator("[data-test-id=animate-height-content]")
        .waitFor({ state: "hidden" });

      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );
      expect(contentVisible).toBeFalsy();
    });

    it("Displays then closes content for View 2", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 2')");
      await page.waitForSelector("[data-test-id=animate-height-content]");

      await expect(page.getByText("Text for view 2")).toBeVisible();

      await page.click("button:has-text('Close')");
      await page
        .locator("[data-test-id=animate-height-content]")
        .waitFor({ state: "hidden" });

      const contentVisible = await page.isVisible(
        "[data-test-id=animate-height-content]"
      );
      expect(contentVisible).toBeFalsy();
    });

    it("Switches from view 1 to view 2", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 1')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      await page.click("button:has-text('View 2')");

      await expect(page.getByText("Text for view 2")).toBeVisible();
    });

    it("Switches from view 2 to view 1", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/ViewSwitch.elm`);

      await page.click("button:has-text('View 2')");
      await page.waitForSelector("[data-test-id=animate-height-content]");
      await page.click("button:has-text('View 1')");

      await expect(page.getByText("Text for view 1")).toBeVisible();
    });
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

  describe("FixedAtAuto", () => {
    it("Shows the content by default", async () => {
      await browser.newContext();
      const page = await browser.newPage();
      await page.goto(`${BASE_URI}/FixedAtAuto.elm`);

      const contentVisible = await page.waitForSelector(
        "[data-test-id=animate-height-content]"
      );

      expect(contentVisible).toBeTruthy();
    });
  });
});
