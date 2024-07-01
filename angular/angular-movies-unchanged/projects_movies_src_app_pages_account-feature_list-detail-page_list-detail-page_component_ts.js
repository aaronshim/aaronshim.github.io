"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_component_ts"],{

/***/ 1789:
/*!******************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-detail-page.component.ts ***!
  \******************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./list-detail-page.adapter */ 2951);







function ListDetailPageComponent_h1_3_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](0, "h1");
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const name_r2 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtextInterpolate"](name_r2);
  }
}
function ListDetailPageComponent_ng_container_10_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](1, "a", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const item_r3 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("routerLink", item_r3.link);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtextInterpolate"](item_r3.name);
  }
}
const _c0 = function () {
  return ["../../../account/my-lists"];
};
class ListDetailPageComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__.ListDetailAdapter);
    this.tabs = [{
      name: 'View List',
      link: 'view'
    }, {
      name: 'Edit List',
      link: 'edit'
    }, {
      name: 'Add/Remove Items',
      link: 'add-remove-items'
    }, {
      name: 'Choose Image',
      link: 'image'
    }, {
      name: 'Delete List',
      link: 'delete'
    }];
  }
  static #_ = this.ɵfac = function ListDetailPageComponent_Factory(t) {
    return new (t || ListDetailPageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineComponent"]({
    type: ListDetailPageComponent,
    selectors: [["ct-list-detail-page"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵStandaloneFeature"]],
    decls: 12,
    vars: 4,
    consts: [[4, "rxLet"], [1, "btn", "primary-button", 3, "routerLink"], ["name", "back", "size", "1em"], [1, "tabs"], [4, "rxFor", "rxForOf"], ["routerLinkActive", "active", 1, "tab", 3, "routerLink"]],
    template: function ListDetailPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](0, "\u00CF\n");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](1, "article")(2, "header");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtemplate"](3, ListDetailPageComponent_h1_3_Template, 2, 1, "h1", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](4, "h2");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](5, "TMDB");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](6, "a", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelement"](7, "fast-svg", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](8, "\u00A0 Back ");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]()();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](9, "div", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtemplate"](10, ListDetailPageComponent_ng_container_10_Template, 3, 2, "ng-container", 4);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelement"](11, "router-outlet");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](3);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("rxLet", ctx.adapter.listName$);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](3);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵpureFunction0"](3, _c0));
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](4);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("rxForOf", ctx.tabs);
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_2__.RouterLink, _angular_router__WEBPACK_IMPORTED_MODULE_2__.RouterOutlet, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_3__.RxLet, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__.RxFor, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_5__.FastSvgComponent],
    styles: [".tabs[_ngcontent-%COMP%] {\n  display: flex;\n  margin: 16px 0;\n  border: 1px solid var(--palette-divider);\n  background-color: var(--palette-background-paper);\n  align-items: center;\n  justify-content: space-between;\n}\n\n.tab[_ngcontent-%COMP%] {\n  padding: 0 16px;\n  min-height: 46px;\n  line-height: 46px;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  cursor: pointer;\n  flex-grow: 1;\n  text-align: center;\n}\n.tab[_ngcontent-%COMP%]:hover {\n  background: val(--palette-background-default);\n}\n.tab.active[_ngcontent-%COMP%] {\n  background: var(--palette-background-default);\n}\n\n.btn[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  display: inline-flex;\n  outline: none;\n  padding: 6px 16px;\n  min-width: 96px;\n  min-height: 48px;\n  font-weight: normal;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  border: 1px solid rgba(var(--palette-primary-main-rgb), 0.5);\n  border-radius: var(--theme-borderRadius-m);\n  box-shadow: none;\n  background-color: transparent;\n  cursor: pointer;\n}\n\n.btn__icon[_ngcontent-%COMP%] {\n  width: 24px;\n  height: 24px;\n  margin-left: 8px;\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  color: var(--palette-primary-contrast-text);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-primary-light);\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  background: none;\n  border: none;\n  display: block;\n  text-align: left;\n  height: inherit;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC90YWJzL190YWJzLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9hY2NvdW50LWZlYXR1cmUvbGlzdC1kZXRhaWwtcGFnZS9saXN0LWRldGFpbC1wYWdlLmNvbXBvbmVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvY29tcG9uZW50L2J1dHRvbi9fYnV0dG9uLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtFQUNFLGFBQUE7RUFDQSxjQUFBO0VBQ0Esd0NBQUE7RUFDQSxpREFBQTtFQUNBLG1CQUFBO0VBQ0EsOEJBQUE7QUNDRjs7QURFQTtFQUNFLGVBQUE7RUFDQSxnQkFBQTtFQUNBLGlCQUFBO0VBQ0EseUJBQUE7RUFDQSxrQ0FBQTtFQUNBLGVBQUE7RUFDQSxZQUFBO0VBQ0Esa0JBQUE7QUNDRjtBRENFO0VBQ0UsNkNBQUE7QUNDSjtBREVFO0VBQ0UsNkNBQUE7QUNBSjs7QUN2QkE7RUNpQkUsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsdUJBQUE7RUFoQkEsb0JBQUE7RURBQSxhQUFBO0VBQ0EsaUJBQUE7RUFDQSxlQUFBO0VBQ0EsZ0JBQUE7RUFDQSxtQkFBQTtFQUNBLHlCQUFBO0VBQ0Esa0NBQUE7RUFDQSw0REFBQTtFQUNBLDBDQUFBO0VBQ0EsZ0JBQUE7RUFDQSw2QkFBQTtFQUNBLGVBQUE7QUQ0QkY7O0FDekJBO0VBQ0UsV0FBQTtFQUNBLFlBQUE7RUFDQSxnQkFBQTtBRDRCRjs7QUN6QkE7RUFDRSx1Q0FBQTtFQUNBLDJDQUFBO0FENEJGO0FDMUJFO0VBQ0UsOENBQUE7QUQ0Qko7O0FDeEJBO0VBQ0UsZ0JBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtFQUNBLGdCQUFBO0VBQ0EsZUFBQTtBRDJCRiIsInNvdXJjZXNDb250ZW50IjpbIi50YWJzIHtcbiAgZGlzcGxheTogZmxleDtcbiAgbWFyZ2luOiAxNnB4IDA7XG4gIGJvcmRlcjogMXB4IHNvbGlkIHZhcigtLXBhbGV0dGUtZGl2aWRlcik7XG4gIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogc3BhY2UtYmV0d2Vlbjtcbn1cblxuLnRhYiB7XG4gIHBhZGRpbmc6IDAgMTZweDtcbiAgbWluLWhlaWdodDogNDZweDtcbiAgbGluZS1oZWlnaHQ6IDQ2cHg7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1tZCk7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktZGFyayk7XG4gIGN1cnNvcjogcG9pbnRlcjtcbiAgZmxleC1ncm93OiAxO1xuICB0ZXh0LWFsaWduOiBjZW50ZXI7XG5cbiAgJjpob3ZlciB7XG4gICAgYmFja2dyb3VuZDogdmFsKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLWRlZmF1bHQpO1xuICB9XG5cbiAgJi5hY3RpdmUge1xuICAgIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1kZWZhdWx0KTtcbiAgfVxufVxuIiwiLnRhYnMge1xuICBkaXNwbGF5OiBmbGV4O1xuICBtYXJnaW46IDE2cHggMDtcbiAgYm9yZGVyOiAxcHggc29saWQgdmFyKC0tcGFsZXR0ZS1kaXZpZGVyKTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLXBhcGVyKTtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBzcGFjZS1iZXR3ZWVuO1xufVxuXG4udGFiIHtcbiAgcGFkZGluZzogMCAxNnB4O1xuICBtaW4taGVpZ2h0OiA0NnB4O1xuICBsaW5lLWhlaWdodDogNDZweDtcbiAgZm9udC1zaXplOiB2YXIoLS10ZXh0LW1kKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1kYXJrKTtcbiAgY3Vyc29yOiBwb2ludGVyO1xuICBmbGV4LWdyb3c6IDE7XG4gIHRleHQtYWxpZ246IGNlbnRlcjtcbn1cbi50YWI6aG92ZXIge1xuICBiYWNrZ3JvdW5kOiB2YWwoLS1wYWxldHRlLWJhY2tncm91bmQtZGVmYXVsdCk7XG59XG4udGFiLmFjdGl2ZSB7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1kZWZhdWx0KTtcbn1cblxuLmJ0biB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbiAgb3V0bGluZTogbm9uZTtcbiAgcGFkZGluZzogNnB4IDE2cHg7XG4gIG1pbi13aWR0aDogOTZweDtcbiAgbWluLWhlaWdodDogNDhweDtcbiAgZm9udC13ZWlnaHQ6IG5vcm1hbDtcbiAgZm9udC1zaXplOiB2YXIoLS10ZXh0LW1kKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1kYXJrKTtcbiAgYm9yZGVyOiAxcHggc29saWQgcmdiYSh2YXIoLS1wYWxldHRlLXByaW1hcnktbWFpbi1yZ2IpLCAwLjUpO1xuICBib3JkZXItcmFkaXVzOiB2YXIoLS10aGVtZS1ib3JkZXJSYWRpdXMtbSk7XG4gIGJveC1zaGFkb3c6IG5vbmU7XG4gIGJhY2tncm91bmQtY29sb3I6IHRyYW5zcGFyZW50O1xuICBjdXJzb3I6IHBvaW50ZXI7XG59XG5cbi5idG5fX2ljb24ge1xuICB3aWR0aDogMjRweDtcbiAgaGVpZ2h0OiAyNHB4O1xuICBtYXJnaW4tbGVmdDogOHB4O1xufVxuXG4ucHJpbWFyeS1idXR0b24ge1xuICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbWFpbik7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktY29udHJhc3QtdGV4dCk7XG59XG4ucHJpbWFyeS1idXR0b246aG92ZXIge1xuICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xufVxuXG4uZnVuY3Rpb25hbGl0eS1vbmx5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IG5vbmU7XG4gIGJvcmRlcjogbm9uZTtcbiAgZGlzcGxheTogYmxvY2s7XG4gIHRleHQtYWxpZ246IGxlZnQ7XG4gIGhlaWdodDogaW5oZXJpdDtcbn0iLCJAaW1wb3J0ICcuLi8uLi90b2tlbi9taXhpbnMvZmxleCc7XG4uYnRuIHtcbiAgQGluY2x1ZGUgZC1mbGV4LXZoO1xuICBAaW5jbHVkZSBkLWlubGluZS1mbGV4O1xuICBvdXRsaW5lOiBub25lO1xuICBwYWRkaW5nOiA2cHggMTZweDtcbiAgbWluLXdpZHRoOiA5NnB4O1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBmb250LXdlaWdodDogbm9ybWFsO1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWRhcmspO1xuICBib3JkZXI6IDFweCBzb2xpZCByZ2JhKHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluLXJnYiksIDAuNSk7XG4gIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1tKTtcbiAgYm94LXNoYWRvdzogbm9uZTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQ7XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cblxuLmJ0bl9faWNvbiB7XG4gIHdpZHRoOiAyNHB4O1xuICBoZWlnaHQ6IDI0cHg7XG4gIG1hcmdpbi1sZWZ0OiA4cHg7XG59XG5cbi5wcmltYXJ5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1jb250cmFzdC10ZXh0KTtcblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xuICB9XG59XG5cbi5mdW5jdGlvbmFsaXR5LW9ubHktYnV0dG9uIHtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBkaXNwbGF5OiBibG9jaztcbiAgdGV4dC1hbGlnbjogbGVmdDtcbiAgaGVpZ2h0OiBpbmhlcml0O1xufVxuIiwiQG1peGluIGQtZmxleCB7XG4gIGRpc3BsYXk6IGZsZXg7XG59XG5AbWl4aW4gZC1pbmxpbmUtZmxleCB7XG4gIGRpc3BsYXk6IGlubGluZS1mbGV4O1xufVxuXG5AbWl4aW4gZC1mbGV4LXYge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LWgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC12aCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ListDetailPageComponent);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_component_ts.js.map