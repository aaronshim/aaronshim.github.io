"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_list-detail-page_list-remove_list-remove_component_ts"],{

/***/ 660:
/*!*************************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-remove/list-remove.component.ts ***!
  \*************************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 7835);
/* harmony import */ var _list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../list-detail-page.adapter */ 2951);







const _c0 = ["dialog"];
class ListRemoveComponent extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  constructor(actionsF) {
    super();
    this.actionsF = actionsF;
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__.ListDetailAdapter);
    this.ui = this.actionsF.create();
    this.hold(this.ui.confirm$, this.adapter.ui.deleteList);
  }
  ngAfterViewInit() {
    this.hold((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.merge)(this.ui.confirm$, this.ui.closeDialog$), () => this.dialog.nativeElement.close());
    this.hold(this.ui.openDialog$, () => this.dialog.nativeElement.showModal());
  }
  ngOnDestroy() {
    super.ngOnDestroy();
  }
  static #_ = this.ɵfac = function ListRemoveComponent_Factory(t) {
    return new (t || ListRemoveComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: ListRemoveComponent,
    selectors: [["app-list-remove"]],
    viewQuery: function ListRemoveComponent_Query(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵviewQuery"](_c0, 7);
      }
      if (rf & 2) {
        let _t;
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵqueryRefresh"](_t = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵloadQuery"]()) && (ctx.dialog = _t.first);
      }
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵProvidersFeature"]([_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory]), _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵInheritDefinitionFeature"], _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵStandaloneFeature"]],
    decls: 16,
    vars: 0,
    consts: [["for", "delete-list-button"], ["id", "delete-list-button", 1, "primary-button", 3, "click"], ["dialog", ""], ["aria-label", "Cancel list delete", 1, "secondary-button", 3, "click"], ["aria-label", "Confirm list delete", 1, "secondary-button", 3, "click"]],
    template: function ListRemoveComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "label", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](1, "Click the button below if you are sure you want to delete this list.");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](2, "button", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListRemoveComponent_Template_button_click_2_listener() {
          return ctx.ui.openDialog();
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](3, " Delete\n");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](4, "dialog", null, 2)(6, "header")(7, "h2");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](8, "Are you sure?");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](9, "p");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](10, "By clicking yes, this list will be permanently deleted.");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](11, "footer")(12, "button", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListRemoveComponent_Template_button_click_12_listener() {
          return ctx.ui.closeDialog();
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](13, " No ");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](14, "button", 4);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListRemoveComponent_Template_button_click_14_listener() {
          return ctx.ui.confirm();
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](15, " Yes ");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()()();
      }
    },
    styles: ["label[_ngcontent-%COMP%] {\n  display: block;\n  font-size: var(--text-md);\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  font-size: var(--text-md);\n  width: 138px;\n  margin-top: 8px;\n  border: none;\n  min-height: 48px;\n  cursor: pointer;\n  border-radius: 5px;\n  transition: transform 100ms ease-in-out;\n  box-shadow: var(--theme-shadow-1);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background: var(--palette-primary-light);\n  transform: translateY(-1px);\n  box-shadow: none;\n}\n\ndialog[_ngcontent-%COMP%] {\n  background: var(--palette-background-paper);\n}\n\ndialog[_ngcontent-%COMP%]::backdrop {\n  background: var(--palette-background-backdrop, rgba(0, 0, 0, 0.5));\n}\n\n.secondary-button[_ngcontent-%COMP%] {\n  border: none;\n  background: none;\n  color: var(--palette-primary-main);\n  font-weight: bold;\n  font-size: var(--text-lg);\n  padding: 4px 8px;\n}\n.secondary-button[_ngcontent-%COMP%]:hover {\n  cursor: pointer;\n  background: var(--palette-action-hover);\n}\n\np[_ngcontent-%COMP%] {\n  font-size: 1.25rem;\n}\n\nheader[_ngcontent-%COMP%], p[_ngcontent-%COMP%], footer[_ngcontent-%COMP%] {\n  padding: 16px;\n  margin-bottom: 0;\n}\n\nheader[_ngcontent-%COMP%], p[_ngcontent-%COMP%] {\n  border-bottom: 1px solid var(--palette-divider);\n}\n\nfooter[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: flex-end;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3BhZ2VzL2FjY291bnQtZmVhdHVyZS9saXN0LWRldGFpbC1wYWdlL2xpc3QtcmVtb3ZlL2xpc3QtcmVtb3ZlLmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0UsY0FBQTtFQUNBLHlCQUFBO0FBQ0Y7O0FBRUE7RUFDRSx1Q0FBQTtFQUNBLHlCQUFBO0VBQ0EsWUFBQTtFQUNBLGVBQUE7RUFDQSxZQUFBO0VBQ0EsZ0JBQUE7RUFDQSxlQUFBO0VBQ0Esa0JBQUE7RUFDQSx1Q0FBQTtFQUNBLGlDQUFBO0FBQ0Y7QUFDRTtFQUNFLHdDQUFBO0VBQ0EsMkJBQUE7RUFDQSxnQkFBQTtBQUNKOztBQUdBO0VBQ0UsMkNBQUE7QUFBRjs7QUFHQTtFQUNFLGtFQUFBO0FBQUY7O0FBR0E7RUFDRSxZQUFBO0VBQ0EsZ0JBQUE7RUFDQSxrQ0FBQTtFQUNBLGlCQUFBO0VBQ0EseUJBQUE7RUFDQSxnQkFBQTtBQUFGO0FBRUU7RUFDRSxlQUFBO0VBQ0EsdUNBQUE7QUFBSjs7QUFJQTtFQUNFLGtCQUFBO0FBREY7O0FBSUE7OztFQUdFLGFBQUE7RUFDQSxnQkFBQTtBQURGOztBQUlBOztFQUVFLCtDQUFBO0FBREY7O0FBSUE7RUFDRSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSx5QkFBQTtBQURGIiwic291cmNlc0NvbnRlbnQiOlsibGFiZWwge1xuICBkaXNwbGF5OiBibG9jaztcbiAgZm9udC1zaXplOiB2YXIoLS10ZXh0LW1kKTtcbn1cblxuLnByaW1hcnktYnV0dG9uIHtcbiAgYmFja2dyb3VuZDogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LW1haW4pO1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICB3aWR0aDogMTM4cHg7XG4gIG1hcmdpbi10b3A6IDhweDtcbiAgYm9yZGVyOiBub25lO1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIGJvcmRlci1yYWRpdXM6IDVweDtcbiAgdHJhbnNpdGlvbjogdHJhbnNmb3JtIDEwMG1zIGVhc2UtaW4tb3V0O1xuICBib3gtc2hhZG93OiB2YXIoLS10aGVtZS1zaGFkb3ctMSk7XG5cbiAgJjpob3ZlciB7XG4gICAgYmFja2dyb3VuZDogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWxpZ2h0KTtcbiAgICB0cmFuc2Zvcm06IHRyYW5zbGF0ZVkoLTFweCk7XG4gICAgYm94LXNoYWRvdzogbm9uZTtcbiAgfVxufVxuXG5kaWFsb2cge1xuICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtcGFwZXIpO1xufVxuXG5kaWFsb2c6OmJhY2tkcm9wIHtcbiAgYmFja2dyb3VuZDogdmFyKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLWJhY2tkcm9wLCByZ2JhKDAsIDAsIDAsIDAuNSkpO1xufVxuXG4uc2Vjb25kYXJ5LWJ1dHRvbiB7XG4gIGJvcmRlcjogbm9uZTtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbiAgZm9udC13ZWlnaHQ6IGJvbGQ7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1sZyk7XG4gIHBhZGRpbmc6IDRweCA4cHg7XG5cbiAgJjpob3ZlciB7XG4gICAgY3Vyc29yOiBwb2ludGVyO1xuICAgIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtYWN0aW9uLWhvdmVyKTtcbiAgfVxufVxuXG5wIHtcbiAgZm9udC1zaXplOiAxLjI1cmVtO1xufVxuXG5oZWFkZXIsXG5wLFxuZm9vdGVyIHtcbiAgcGFkZGluZzogMTZweDtcbiAgbWFyZ2luLWJvdHRvbTogMDtcbn1cblxuaGVhZGVyLFxucCB7XG4gIGJvcmRlci1ib3R0b206IDFweCBzb2xpZCB2YXIoLS1wYWxldHRlLWRpdmlkZXIpO1xufVxuXG5mb290ZXIge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGZsZXgtZW5kO1xufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ListRemoveComponent);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_list-detail-page_list-remove_list-remove_component_ts.js.map