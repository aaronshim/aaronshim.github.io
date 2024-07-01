"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_not-found-page_not-found-page_component_ts"],{

/***/ 8884:
/*!**********************************************************************************!*\
  !*** ./projects/movies/src/app/pages/not-found-page/not-found-page.component.ts ***!
  \**********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);


class NotFoundPageComponent {
  static #_ = this.ɵfac = function NotFoundPageComponent_Factory(t) {
    return new (t || NotFoundPageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: NotFoundPageComponent,
    selectors: [["ct-not-found"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    decls: 6,
    vars: 0,
    consts: [[1, "not-found-container"], ["size", "350px", "name", "error"], [1, "title"], ["routerLink", "/list/category/popular", 1, "btn"]],
    template: function NotFoundPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "div", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelement"](1, "fast-svg", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](2, "h1", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtext"](3, "Sorry, page not found");
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](4, "a", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtext"](5, "See popular");
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]()();
      }
    },
    dependencies: [_push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_1__.FastSvgComponent],
    styles: [".btn[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  display: inline-flex;\n  outline: none;\n  padding: 6px 16px;\n  min-width: 96px;\n  min-height: 48px;\n  font-weight: normal;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  border: 1px solid rgba(var(--palette-primary-main-rgb), 0.5);\n  border-radius: var(--theme-borderRadius-m);\n  box-shadow: none;\n  background-color: transparent;\n  cursor: pointer;\n}\n\n.btn__icon[_ngcontent-%COMP%] {\n  width: 24px;\n  height: 24px;\n  margin-left: 8px;\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  color: var(--palette-primary-contrast-text);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-primary-light);\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  background: none;\n  border: none;\n  display: block;\n  text-align: left;\n  height: inherit;\n}\n\n[_nghost-%COMP%] {\n  width: 100%;\n  height: 100%;\n  display: block;\n}\n\n.not-found-container[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  flex-direction: column;\n}\n\n.title[_ngcontent-%COMP%] {\n  text-align: center;\n  font-size: 4rem;\n  font-weight: 700;\n  margin: 3rem 1rem;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9idXR0b24vX2J1dHRvbi5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9ub3QtZm91bmQtcGFnZS9ub3QtZm91bmQtcGFnZS5jb21wb25lbnQuc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFDQTtFQ2lCRSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSx1QkFBQTtFQWhCQSxvQkFBQTtFREFBLGFBQUE7RUFDQSxpQkFBQTtFQUNBLGVBQUE7RUFDQSxnQkFBQTtFQUNBLG1CQUFBO0VBQ0EseUJBQUE7RUFDQSxrQ0FBQTtFQUNBLDREQUFBO0VBQ0EsMENBQUE7RUFDQSxnQkFBQTtFQUNBLDZCQUFBO0VBQ0EsZUFBQTtBRUVGOztBRkNBO0VBQ0UsV0FBQTtFQUNBLFlBQUE7RUFDQSxnQkFBQTtBRUVGOztBRkNBO0VBQ0UsdUNBQUE7RUFDQSwyQ0FBQTtBRUVGO0FGQUU7RUFDRSw4Q0FBQTtBRUVKOztBRkVBO0VBQ0UsZ0JBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtFQUNBLGdCQUFBO0VBQ0EsZUFBQTtBRUNGOztBQXBDQTtFQUNFLFdBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtBQXVDRjs7QUFwQ0E7RURTRSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSx1QkFBQTtFQ1RBLHNCQUFBO0FBeUNGOztBQXRDQTtFQUNFLGtCQUFBO0VBQ0EsZUFBQTtFQUNBLGdCQUFBO0VBQ0EsaUJBQUE7QUF5Q0YiLCJzb3VyY2VzQ29udGVudCI6WyJAaW1wb3J0ICcuLi8uLi90b2tlbi9taXhpbnMvZmxleCc7XG4uYnRuIHtcbiAgQGluY2x1ZGUgZC1mbGV4LXZoO1xuICBAaW5jbHVkZSBkLWlubGluZS1mbGV4O1xuICBvdXRsaW5lOiBub25lO1xuICBwYWRkaW5nOiA2cHggMTZweDtcbiAgbWluLXdpZHRoOiA5NnB4O1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBmb250LXdlaWdodDogbm9ybWFsO1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWRhcmspO1xuICBib3JkZXI6IDFweCBzb2xpZCByZ2JhKHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluLXJnYiksIDAuNSk7XG4gIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1tKTtcbiAgYm94LXNoYWRvdzogbm9uZTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQ7XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cblxuLmJ0bl9faWNvbiB7XG4gIHdpZHRoOiAyNHB4O1xuICBoZWlnaHQ6IDI0cHg7XG4gIG1hcmdpbi1sZWZ0OiA4cHg7XG59XG5cbi5wcmltYXJ5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1jb250cmFzdC10ZXh0KTtcblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xuICB9XG59XG5cbi5mdW5jdGlvbmFsaXR5LW9ubHktYnV0dG9uIHtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBkaXNwbGF5OiBibG9jaztcbiAgdGV4dC1hbGlnbjogbGVmdDtcbiAgaGVpZ2h0OiBpbmhlcml0O1xufVxuIiwiQG1peGluIGQtZmxleCB7XG4gIGRpc3BsYXk6IGZsZXg7XG59XG5AbWl4aW4gZC1pbmxpbmUtZmxleCB7XG4gIGRpc3BsYXk6IGlubGluZS1mbGV4O1xufVxuXG5AbWl4aW4gZC1mbGV4LXYge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LWgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC12aCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuIiwiQGltcG9ydCAnLi4vLi4vdWkvY29tcG9uZW50L2J1dHRvbi9idXR0b24nO1xuQGltcG9ydCAnLi4vLi4vdWkvdG9rZW4vbWl4aW5zL2ZsZXgnO1xuXG46aG9zdCB7XG4gIHdpZHRoOiAxMDAlO1xuICBoZWlnaHQ6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xufVxuXG4ubm90LWZvdW5kLWNvbnRhaW5lciB7XG4gIEBpbmNsdWRlIGQtZmxleC12aDtcbiAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbn1cblxuLnRpdGxlIHtcbiAgdGV4dC1hbGlnbjogY2VudGVyO1xuICBmb250LXNpemU6IDRyZW07XG4gIGZvbnQtd2VpZ2h0OiA3MDA7XG4gIG1hcmdpbjogM3JlbSAxcmVtO1xufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (NotFoundPageComponent);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_not-found-page_not-found-page_component_ts.js.map