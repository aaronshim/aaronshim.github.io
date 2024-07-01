"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_app-shell_account-menu_account-menu_component_ts"],{

/***/ 3559:
/*!**********************************************************************************!*\
  !*** ./projects/movies/src/app/app-shell/account-menu/account-menu.component.ts ***!
  \**********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__),
/* harmony export */   imports: () => (/* binding */ imports)
/* harmony export */ });
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _auth_auth_effects__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../auth/auth.effects */ 3069);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/state/effects */ 7448);
/* harmony import */ var _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/template/if */ 1989);
/* harmony import */ var _state_account_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../state/account.state */ 3412);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);










const _c0 = function () {
  return ["account/list/create"];
};
const _c1 = function () {
  return ["account/my-lists"];
};
function AccountMenuComponent_ng_container_0_Template(rf, ctx) {
  if (rf & 1) {
    const _r4 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](1, "div")(2, "a", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](3, "Create New List");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](4, "div")(5, "a", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](6, "My Lists");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](7, "div")(8, "a", 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function AccountMenuComponent_ng_container_0_Template_a_click_8_listener($event) {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r4);
      const ctx_r3 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r3.ui.signOut($event));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](9, " Logout ");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵpureFunction0"](3, _c0));
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵpureFunction0"](4, _c1));
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("routerLink", "/list/category/popular");
  }
}
function AccountMenuComponent_ng_template_1_Template(rf, ctx) {
  if (rf & 1) {
    const _r6 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "div")(1, "p");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](2, "Guest Profile");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](3, "div")(4, "button", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function AccountMenuComponent_ng_template_1_Template_button_click_4_listener($event) {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r6);
      const ctx_r5 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r5.ui.signIn($event));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](5, " Login ");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
  }
}
const imports = [_angular_router__WEBPACK_IMPORTED_MODULE_3__.RouterLink, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_4__.RxLet, _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_5__.RxIf];
class AccountMenuComponent {
  constructor(actionsF) {
    this.actionsF = actionsF;
    this.effects = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_6__.RxEffects);
    this.authEffects = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_auth_auth_effects__WEBPACK_IMPORTED_MODULE_0__.AuthEffects);
    this.accountState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_state_account_state__WEBPACK_IMPORTED_MODULE_1__.AccountState);
    this.state = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_rx_angular_state__WEBPACK_IMPORTED_MODULE_7__.RxState);
    this.ui = this.actionsF.create();
    this.loggedIn$ = this.state.select('loggedIn');
    this.state.connect('loggedIn', this.accountState.loggedIn$);
    this.effects.register(this.ui.signOut$, this.authEffects.signOut);
    this.effects.register(this.ui.signIn$, this.authEffects.signInStart);
  }
  static #_ = this.ɵfac = function AccountMenuComponent_Factory(t) {
    return new (t || AccountMenuComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_8__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: AccountMenuComponent,
    selectors: [["ct-account-menu"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵProvidersFeature"]([_rx_angular_state__WEBPACK_IMPORTED_MODULE_7__.RxState, _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_6__.RxEffects]), _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵStandaloneFeature"]],
    decls: 3,
    vars: 2,
    consts: [[4, "rxIf", "rxIfElse"], ["guest", ""], [3, "routerLink"], ["data-uf", "profile-menu-item-signout", "title", "sign out", 3, "routerLink", "click"], ["type", "button", "data-uf", "profile-menu-item-login", "title", "sign in", 1, "functionality-only-button", 3, "click"]],
    template: function AccountMenuComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](0, AccountMenuComponent_ng_container_0_Template, 10, 5, "ng-container", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](1, AccountMenuComponent_ng_template_1_Template, 6, 0, "ng-template", null, 1, _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplateRefExtractor"]);
      }
      if (rf & 2) {
        const _r1 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵreference"](2);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxIf", ctx.loggedIn$)("rxIfElse", _r1);
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_3__.RouterLink, _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_5__.RxIf],
    styles: [".btn[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  display: inline-flex;\n  outline: none;\n  padding: 6px 16px;\n  min-width: 96px;\n  min-height: 48px;\n  font-weight: normal;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  border: 1px solid rgba(var(--palette-primary-main-rgb), 0.5);\n  border-radius: var(--theme-borderRadius-m);\n  box-shadow: none;\n  background-color: transparent;\n  cursor: pointer;\n}\n\n.btn__icon[_ngcontent-%COMP%] {\n  width: 24px;\n  height: 24px;\n  margin-left: 8px;\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  color: var(--palette-primary-contrast-text);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-primary-light);\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  background: none;\n  border: none;\n  display: block;\n  text-align: left;\n  height: inherit;\n}\n\n[_nghost-%COMP%] {\n  display: flex;\n  flex-direction: column;\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  width: 100%;\n  padding: 0 15px;\n}\n\ndiv[_ngcontent-%COMP%] {\n  font-size: var(--text-md);\n  padding: 0;\n  cursor: pointer;\n  display: flex;\n  align-items: center;\n  color: var(--palette-text-primary);\n  height: 36px;\n  background-color: var(--palette-background-paper);\n  transition: background-color 150ms cubic-bezier(0.4, 0, 0.2, 1) 0s;\n}\ndiv[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-action-hover);\n}\ndiv[_ngcontent-%COMP%]   a[_ngcontent-%COMP%], div[_ngcontent-%COMP%]   p[_ngcontent-%COMP%] {\n  color: var(--palette-text-primary);\n  height: 100%;\n  display: inline-block;\n  width: 100%;\n  padding: 6px 16px;\n}\n\ndiv[_ngcontent-%COMP%]:not(:last-child) {\n  border-bottom: 1px solid var(--palette-divider);\n}\n\na[_ngcontent-%COMP%] {\n  font-size: var(--text-md);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9idXR0b24vX2J1dHRvbi5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9hcHAtc2hlbGwvYWNjb3VudC1tZW51L2FjY291bnQtbWVudS5jb21wb25lbnQuc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFDQTtFQ2lCRSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSx1QkFBQTtFQWhCQSxvQkFBQTtFREFBLGFBQUE7RUFDQSxpQkFBQTtFQUNBLGVBQUE7RUFDQSxnQkFBQTtFQUNBLG1CQUFBO0VBQ0EseUJBQUE7RUFDQSxrQ0FBQTtFQUNBLDREQUFBO0VBQ0EsMENBQUE7RUFDQSxnQkFBQTtFQUNBLDZCQUFBO0VBQ0EsZUFBQTtBRUVGOztBRkNBO0VBQ0UsV0FBQTtFQUNBLFlBQUE7RUFDQSxnQkFBQTtBRUVGOztBRkNBO0VBQ0UsdUNBQUE7RUFDQSwyQ0FBQTtBRUVGO0FGQUU7RUFDRSw4Q0FBQTtBRUVKOztBRkVBO0VBQ0UsZ0JBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtFQUNBLGdCQUFBO0VBQ0EsZUFBQTtBRUNGOztBQXJDQTtFQUNFLGFBQUE7RUFDQSxzQkFBQTtBQXdDRjs7QUFyQ0E7RUFDRSxXQUFBO0VBQ0EsZUFBQTtBQXdDRjs7QUFyQ0E7RUFDRSx5QkFBQTtFQUNBLFVBQUE7RUFDQSxlQUFBO0VBQ0EsYUFBQTtFQUNBLG1CQUFBO0VBQ0Esa0NBQUE7RUFDQSxZQUFBO0VBQ0EsaURBQUE7RUFDQSxrRUFBQTtBQXdDRjtBQXZDRTtFQUNFLDZDQUFBO0FBeUNKO0FBdkNFOztFQUVFLGtDQUFBO0VBQ0EsWUFBQTtFQUNBLHFCQUFBO0VBQ0EsV0FBQTtFQUNBLGlCQUFBO0FBeUNKOztBQXJDQTtFQUNFLCtDQUFBO0FBd0NGOztBQXJDQTtFQUNFLHlCQUFBO0FBd0NGIiwic291cmNlc0NvbnRlbnQiOlsiQGltcG9ydCAnLi4vLi4vdG9rZW4vbWl4aW5zL2ZsZXgnO1xuLmJ0biB7XG4gIEBpbmNsdWRlIGQtZmxleC12aDtcbiAgQGluY2x1ZGUgZC1pbmxpbmUtZmxleDtcbiAgb3V0bGluZTogbm9uZTtcbiAgcGFkZGluZzogNnB4IDE2cHg7XG4gIG1pbi13aWR0aDogOTZweDtcbiAgbWluLWhlaWdodDogNDhweDtcbiAgZm9udC13ZWlnaHQ6IG5vcm1hbDtcbiAgZm9udC1zaXplOiB2YXIoLS10ZXh0LW1kKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1kYXJrKTtcbiAgYm9yZGVyOiAxcHggc29saWQgcmdiYSh2YXIoLS1wYWxldHRlLXByaW1hcnktbWFpbi1yZ2IpLCAwLjUpO1xuICBib3JkZXItcmFkaXVzOiB2YXIoLS10aGVtZS1ib3JkZXJSYWRpdXMtbSk7XG4gIGJveC1zaGFkb3c6IG5vbmU7XG4gIGJhY2tncm91bmQtY29sb3I6IHRyYW5zcGFyZW50O1xuICBjdXJzb3I6IHBvaW50ZXI7XG59XG5cbi5idG5fX2ljb24ge1xuICB3aWR0aDogMjRweDtcbiAgaGVpZ2h0OiAyNHB4O1xuICBtYXJnaW4tbGVmdDogOHB4O1xufVxuXG4ucHJpbWFyeS1idXR0b24ge1xuICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbWFpbik7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktY29udHJhc3QtdGV4dCk7XG5cbiAgJjpob3ZlciB7XG4gICAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWxpZ2h0KTtcbiAgfVxufVxuXG4uZnVuY3Rpb25hbGl0eS1vbmx5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IG5vbmU7XG4gIGJvcmRlcjogbm9uZTtcbiAgZGlzcGxheTogYmxvY2s7XG4gIHRleHQtYWxpZ246IGxlZnQ7XG4gIGhlaWdodDogaW5oZXJpdDtcbn1cbiIsIkBtaXhpbiBkLWZsZXgge1xuICBkaXNwbGF5OiBmbGV4O1xufVxuQG1peGluIGQtaW5saW5lLWZsZXgge1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbn1cblxuQG1peGluIGQtZmxleC12IHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC1oIHtcbiAgZGlzcGxheTogZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtdmgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cbiIsIkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9idXR0b24vX2J1dHRvbic7XG5cbjpob3N0IHtcbiAgZGlzcGxheTogZmxleDtcbiAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbn1cblxuLmZ1bmN0aW9uYWxpdHktb25seS1idXR0b24ge1xuICB3aWR0aDogMTAwJTtcbiAgcGFkZGluZzogMCAxNXB4O1xufVxuXG5kaXYge1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBwYWRkaW5nOiAwO1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXRleHQtcHJpbWFyeSk7XG4gIGhlaWdodDogMzZweDtcbiAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLXBhcGVyKTtcbiAgdHJhbnNpdGlvbjogYmFja2dyb3VuZC1jb2xvciAxNTBtcyBjdWJpYy1iZXppZXIoMC40LCAwLCAwLjIsIDEpIDBzO1xuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLWFjdGlvbi1ob3Zlcik7XG4gIH1cbiAgYSxcbiAgcCB7XG4gICAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1wcmltYXJ5KTtcbiAgICBoZWlnaHQ6IDEwMCU7XG4gICAgZGlzcGxheTogaW5saW5lLWJsb2NrO1xuICAgIHdpZHRoOiAxMDAlO1xuICAgIHBhZGRpbmc6IDZweCAxNnB4O1xuICB9XG59XG5cbmRpdjpub3QoOmxhc3QtY2hpbGQpIHtcbiAgYm9yZGVyLWJvdHRvbTogMXB4IHNvbGlkIHZhcigtLXBhbGV0dGUtZGl2aWRlcik7XG59XG5cbmEge1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (AccountMenuComponent);

/***/ }),

/***/ 3069:
/*!******************************************************!*\
  !*** ./projects/movies/src/app/auth/auth.effects.ts ***!
  \******************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AuthEffects: () => (/* binding */ AuthEffects)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _data_access_api_resources_authv4_resource__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../data-access/api/resources/authv4.resource */ 865);
/* harmony import */ var _access_token_facade_service__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./access-token-facade.service */ 6641);
/* harmony import */ var _state_account_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../state/account.state */ 3412);






class AuthEffects {
  constructor() {
    this.document = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_4__.DOCUMENT);
    this.platformId = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_3__.PLATFORM_ID);
    this.authResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_data_access_api_resources_authv4_resource__WEBPACK_IMPORTED_MODULE_0__.Authv4Resource);
    this.accessTokenFacade = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_access_token_facade_service__WEBPACK_IMPORTED_MODULE_1__.AccessTokenFacade);
    this.accountState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_state_account_state__WEBPACK_IMPORTED_MODULE_2__.AccountState);
    this.redirectUrl = `${this.document.location.protocol}//${this.document.location.hostname}:${this.document.location.port}/list/category/popular`;
    this.signInStart = () => {
      this.authResource.createRequestToken(this.redirectUrl).subscribe(({
        request_token
      }) => {
        if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_4__.isPlatformBrowser)(this.platformId)) {
          // after redirecting to the redirectUrl, the requestToken in localStorage will indicate that an accessToken should be requested
          window.localStorage.setItem('requestToken', request_token);
        }
        this.document.location.replace(`https://www.themoviedb.org/auth/access?request_token=${request_token}`);
      });
    };
    this.signInFinish = requestToken => {
      this.authResource.createAccessToken(requestToken).subscribe(({
        access_token,
        account_id
      }) => {
        if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_4__.isPlatformBrowser)(this.platformId)) {
          window.localStorage.removeItem('requestToken');
          window.localStorage.setItem('accountId', account_id);
          this.accountState.set({
            accountId: account_id
          });
          window.localStorage.setItem('accessToken', access_token);
          this.accessTokenFacade.setUserAccessToken(access_token);
        }
      });
    };
    this.signOut = () => {
      const accessToken = window.localStorage.getItem('accessToken');
      if (accessToken) {
        this.authResource.deleteAccessToken(accessToken).subscribe();
      }
      window.localStorage.clear();
      this.accountState.set({
        accountId: null
      });
      this.accessTokenFacade.resetToReadAccessToken();
    };
    if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_4__.isPlatformBrowser)(this.platformId)) {
      // should we finish the signIn ?
      const requestToken = window.localStorage.getItem('requestToken');
      requestToken && this.signInFinish(requestToken);
    }
  }
  static #_ = this.ɵfac = function AuthEffects_Factory(t) {
    return new (t || AuthEffects)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjectable"]({
    token: AuthEffects,
    factory: AuthEffects.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 865:
/*!******************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/authv4.resource.ts ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   Authv4Resource: () => (/* binding */ Authv4Resource)
/* harmony export */ });
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ 4860);




const baseUrl = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV4, 'auth'].join('/');
const URL_REQUEST_TOKEN = [baseUrl, 'request_token'].join('/');
const URL_ACCESS_TOKEN = [baseUrl, 'access_token'].join('/');
class Authv4Resource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_2__.HttpClient);
    this.createRequestToken = redirect_to => this.http.post(URL_REQUEST_TOKEN, {
      redirect_to
    });
    this.createAccessToken = requestToken => this.http.post(URL_ACCESS_TOKEN, {
      request_token: requestToken
    });
    this.deleteAccessToken = access_token => this.http.delete(URL_ACCESS_TOKEN, {
      body: {
        access_token
      }
    });
  }
  static #_ = this.ɵfac = function Authv4Resource_Factory(t) {
    return new (t || Authv4Resource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: Authv4Resource,
    factory: Authv4Resource.ɵfac,
    providedIn: 'root'
  });
}


/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_app-shell_account-menu_account-menu_component_ts.js.map