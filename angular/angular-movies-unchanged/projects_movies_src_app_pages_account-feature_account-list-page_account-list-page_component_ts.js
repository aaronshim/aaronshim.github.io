"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_account-list-page_account-list-page_component_ts"],{

/***/ 5172:
/*!****************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/list.resource.ts ***!
  \****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListResource: () => (/* binding */ ListResource)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ 4860);





const URL_LIST_BASE = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV4, 'list'].join('/');
const URL_EXISTING_LIST = id => [URL_LIST_BASE, id].join('/');
const URL_ADD_MOVIE_TO_LIST = id => [URL_EXISTING_LIST(id), 'items'].join('/');
class ListResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_2__.HttpClient);
    this.createList = params => this.http.post(URL_LIST_BASE, params).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(({
      id
    }) => id));
    this.fetchList = id => this.http.get(URL_EXISTING_LIST(+id)).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(list => ({
      [id]: list
    })));
    this.updateList = params => this.http.put(URL_EXISTING_LIST(params.id || 0), params);
    this.addMovieToList = params => this.http.post(URL_ADD_MOVIE_TO_LIST(params.id), params);
    this.deleteMovieFromList = params => this.http.delete(URL_ADD_MOVIE_TO_LIST(params.id), {
      body: params
    });
    this.deleteList = id => this.http.delete(URL_EXISTING_LIST(+id));
  }
  static #_ = this.ɵfac = function ListResource_Factory(t) {
    return new (t || ListResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: ListResource,
    factory: ListResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 3178:
/*!******************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/account-list-page/account-list-page.adapter.ts ***!
  \******************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AccountListPageAdapter: () => (/* binding */ AccountListPageAdapter)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../data-access/images/image-sizes */ 4082);
/* harmony import */ var _state_account_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../../state/account.state */ 3412);
/* harmony import */ var _state_list_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../state/list.state */ 7219);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../../shared/cdk/image/image-tag.transform */ 9699);
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../../constants */ 4765);









class AccountListPageAdapter extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_5__.RxState {
  constructor() {
    super();
    this.list = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_state_list_state__WEBPACK_IMPORTED_MODULE_2__.ListState);
    const accountState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_state_account_state__WEBPACK_IMPORTED_MODULE_1__.AccountState);
    this.connect('lists', accountState.accountLists$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(lists => lists.map(l => (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(l, {
      pathProp: 'backdrop_path',
      dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W500H282,
      fallback: _constants__WEBPACK_IMPORTED_MODULE_4__.MY_LIST_FALLBACK
    })))));
    this.connect('lists', this.list.deleteListSignal$, (state, id) => state.lists?.filter(l => l.id !== +id));
  }
  static #_ = this.ɵfac = function AccountListPageAdapter_Factory(t) {
    return new (t || AccountListPageAdapter)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_6__["ɵɵdefineInjectable"]({
    token: AccountListPageAdapter,
    factory: AccountListPageAdapter.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 9689:
/*!********************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/account-list-page/account-list-page.component.ts ***!
  \********************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../shared/cdk/track-by */ 5221);
/* harmony import */ var _account_list_page_adapter__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./account-list-page.adapter */ 3178);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _ui_component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../ui/component/grid-list/grid-list.component */ 7866);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/common */ 6575);








const _c0 = function (a1, a2) {
  return ["/detail/list", a1, a2];
};
function AccountListPageComponent_ng_container_7_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](1, "a", 1);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelement"](2, "img", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](3, "div", 3)(4, "h3", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](5);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](6, "p", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](7);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]()()();
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const list_r1 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵpureFunction2"](10, _c0, list_r1.id, list_r1.number_of_items ? "view" : "add-remove-items"));
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵproperty"]("ngSrc", list_r1.imgSrc)("ngSrcset", "154w, 185w, 342w, 500w, 780w")("width", list_r1.imgWidth)("height", list_r1.imgHeight)("alt", list_r1.name)("title", list_r1.name);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtextInterpolate1"](" ", list_r1.name, " ");
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtextInterpolate2"](" ", list_r1.number_of_items, " items (", list_r1.public ? "Public" : "Private", ") ");
  }
}
class AccountListPageComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_account_list_page_adapter__WEBPACK_IMPORTED_MODULE_1__.AccountListPageAdapter);
    this.lists$ = this.adapter.select('lists');
    this.trackById = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByProp)('id');
  }
  static #_ = this.ɵfac = function AccountListPageComponent_Factory(t) {
    return new (t || AccountListPageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineComponent"]({
    type: AccountListPageComponent,
    selectors: [["ct-person"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵStandaloneFeature"]],
    decls: 8,
    vars: 2,
    consts: [[4, "rxFor", "rxForOf", "rxForTrackBy"], [1, "ui-grid-list-item", 3, "routerLink"], [1, "aspectRatio-16-9", "gradient", 3, "ngSrc", "ngSrcset", "width", "height", "alt", "title"], [1, "account-list--details"], [1, "account-list--details-title"], [1, "account-list--details-stats"]],
    template: function AccountListPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](0, "article")(1, "header")(2, "h1");
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](3, "My Lists");
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](4, "h2");
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](5, "TMDB");
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]()();
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](6, "ui-grid-list");
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtemplate"](7, AccountListPageComponent_ng_container_7_Template, 8, 13, "ng-container", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]()();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](7);
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵproperty"]("rxForOf", ctx.lists$)("rxForTrackBy", ctx.trackById);
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_4__.RouterLink, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_5__.RxFor, _ui_component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_2__.GridListComponent, _angular_common__WEBPACK_IMPORTED_MODULE_6__.NgOptimizedImage],
    styles: [".aspectRatio-2-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-2-3);\n}\n\n.aspectRatio-16-9[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-16-9);\n}\n\n.aspectRatio-4-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-4-3);\n}\n\n.fit-cover[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  object-fit: cover;\n  height: 100%;\n}\n\n.account-list--details[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  flex-direction: column;\n  justify-content: space-between;\n  padding: 1.5rem 3rem;\n}\n@media only screen and (max-width: 500px) {\n  .account-list--details[_ngcontent-%COMP%] {\n    padding: 1.5rem;\n  }\n}\n.account-list--details-title[_ngcontent-%COMP%] {\n  text-align: center;\n  margin-bottom: 1rem;\n  line-height: 1.4;\n}\n.account-list--details-stats[_ngcontent-%COMP%] {\n  color: var(--palette-text-secondary);\n}\n\n.ui-grid-list-item[_ngcontent-%COMP%] {\n  content-visibility: auto;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9hY2NvdW50LWZlYXR1cmUvYWNjb3VudC1saXN0LXBhZ2UvYWNjb3VudC1saXN0LXBhZ2UuY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtFQUNFLFdBQUE7RUFDQSxjQUFBO0VBQ0EsWUFBQTtFQUNBLDBDQUFBO0FDQ0Y7O0FEQ0E7RUFDRSxXQUFBO0VBQ0EsY0FBQTtFQUNBLFlBQUE7RUFDQSwyQ0FBQTtBQ0VGOztBREFBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMENBQUE7QUNHRjs7QURBQTtFQUNFLFdBQUE7RUFDQSxjQUFBO0VBQ0EsaUJBQUE7RUFDQSxZQUFBO0FDR0Y7O0FBdEJFO0VDSUEsYUFBQTtFQUNBLG1CQUFBO0VESEUsc0JBQUE7RUFDQSw4QkFBQTtFQUNBLG9CQUFBO0FBMEJKO0FBekJJO0VBTEY7SUFNSSxlQUFBO0VBNEJKO0FBQ0Y7QUExQkk7RUFDRSxrQkFBQTtFQUNBLG1CQUFBO0VBQ0EsZ0JBQUE7QUE0Qk47QUF6Qkk7RUFDRSxvQ0FBQTtBQTJCTjs7QUF2QkE7RUFDRSx3QkFBQTtBQTBCRiIsInNvdXJjZXNDb250ZW50IjpbIi5hc3BlY3RSYXRpby0yLTMge1xuICB3aWR0aDogMTAwJTtcbiAgZGlzcGxheTogYmxvY2s7XG4gIGhlaWdodDogYXV0bztcbiAgYXNwZWN0LXJhdGlvOiB2YXIoLS10aGVtZS1hc3BlY3RSYXRpby0yLTMpO1xufVxuLmFzcGVjdFJhdGlvLTE2LTkge1xuICB3aWR0aDogMTAwJTtcbiAgZGlzcGxheTogYmxvY2s7XG4gIGhlaWdodDogYXV0bztcbiAgYXNwZWN0LXJhdGlvOiB2YXIoLS10aGVtZS1hc3BlY3RSYXRpby0xNi05KTtcbn1cbi5hc3BlY3RSYXRpby00LTMge1xuICB3aWR0aDogMTAwJTtcbiAgZGlzcGxheTogYmxvY2s7XG4gIGhlaWdodDogYXV0bztcbiAgYXNwZWN0LXJhdGlvOiB2YXIoLS10aGVtZS1hc3BlY3RSYXRpby00LTMpO1xufVxuXG4uZml0LWNvdmVyIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBvYmplY3QtZml0OiBjb3ZlcjtcbiAgaGVpZ2h0OiAxMDAlO1xufVxuIiwiQGltcG9ydCAnLi4vLi4vLi4vdWkvdG9rZW4vbWl4aW5zL2ZsZXgnO1xuQGltcG9ydCAnLi4vLi4vLi4vdWkvY29tcG9uZW50L2FzcGVjdC1yYXRpby9hc3BlY3QtcmF0aW8nO1xuXG4uYWNjb3VudC1saXN0IHtcbiAgJi0tZGV0YWlscyB7XG4gICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAgICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gICAgcGFkZGluZzogMS41cmVtIDNyZW07XG4gICAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA1MDBweCkge1xuICAgICAgcGFkZGluZzogMS41cmVtO1xuICAgIH1cblxuICAgICYtdGl0bGUge1xuICAgICAgdGV4dC1hbGlnbjogY2VudGVyO1xuICAgICAgbWFyZ2luLWJvdHRvbTogMXJlbTtcbiAgICAgIGxpbmUtaGVpZ2h0OiAxLjQ7XG4gICAgfVxuXG4gICAgJi1zdGF0cyB7XG4gICAgICBjb2xvcjogdmFyKC0tcGFsZXR0ZS10ZXh0LXNlY29uZGFyeSk7XG4gICAgfVxuICB9XG59XG4udWktZ3JpZC1saXN0LWl0ZW0ge1xuICBjb250ZW50LXZpc2liaWxpdHk6IGF1dG87XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (AccountListPageComponent);

/***/ }),

/***/ 9699:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/image/image-tag.transform.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addImageTag: () => (/* binding */ addImageTag)
/* harmony export */ });
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../constants */ 4765);

function addImageTag(_res, options) {
  let {
    fallback
  } = options;
  const {
    pathProp,
    dims,
    sizes,
    srcset
  } = options;
  fallback = fallback || _constants__WEBPACK_IMPORTED_MODULE_0__.POSTER_FALLBACK;
  const res = _res;
  if (sizes !== undefined) {
    res.imgSizes = sizes || '';
    res.imgSrcset = srcset || '';
  }
  res.imgSrc = res[pathProp] ? res[pathProp] + '' : fallback;
  res.imgWidth = dims.WIDTH;
  res.imgHeight = dims.HEIGHT;
  res.imgRatio = res.imgWidth / res.imgHeight;
  return res;
}

/***/ }),

/***/ 7219:
/*!*****************************************************!*\
  !*** ./projects/movies/src/app/state/list.state.ts ***!
  \*****************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListState: () => (/* binding */ ListState)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs */ 7835);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 9877);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 3738);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var _data_access_api_resources_list_resource__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../data-access/api/resources/list.resource */ 5172);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);








class ListState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  constructor() {
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.DestroyRef).onDestroy(() => this.actionsF.destroy());
    super();
    this.router = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_router__WEBPACK_IMPORTED_MODULE_3__.Router);
    this.actionsF = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory();
    this.listResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_data_access_api_resources_list_resource__WEBPACK_IMPORTED_MODULE_0__.ListResource);
    this.actions = this.actionsF.create();
    this.createList = this.actions.createList;
    this.fetchList = this.actions.fetchList;
    this.updateList = this.actions.updateList;
    this.addMovieToList = this.actions.addMovieToList;
    this.deleteMovieFromList = this.actions.deleteMovieFromList;
    this.deleteList = this.actions.deleteList;
    this.deleteListSignal$ = this.actions.deleteList$;
    this.sideEffects$ = (0,rxjs__WEBPACK_IMPORTED_MODULE_5__.merge)(this.actions.addMovieToList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(([movie, id]) => this.listResource.addMovieToList({
      id,
      items: [{
        media_id: movie.id,
        media_type: 'movie'
      }]
    }))), this.actions.deleteMovieFromList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(([movie, id]) => this.listResource.deleteMovieFromList({
      id,
      items: [{
        media_id: movie.id || 0,
        media_type: 'movie'
      }]
    }))), this.actions.updateList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(params => this.listResource.updateList(params))), this.actions.createList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(params => this.listResource.createList(params)), (0,rxjs__WEBPACK_IMPORTED_MODULE_7__.tap)(id => id && this.router.navigate(['account/my-lists']))), this.actions.deleteList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.tap)(id => id && this.router.navigate(['account/my-lists'])), (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(id => this.listResource.deleteList(id))));
    this.connect('lists', this.actions.fetchList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.filter)(id => !isNaN(Number(id))), (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(id => this.listResource.fetchList(id))), (state, list) => (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state?.lists || {}, list));
    this.connect('lists', this.actions.updateList$, (state, update) => {
      if (state && update.id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [update.id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[update.id], update)
        });
      }
      return state.lists;
    });
    this.connect('lists', this.actions.addMovieToList$, (state, [movie, id]) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[id], {
            results: [...(state.lists[id].results || []), movie]
          })
        });
      }
      return state.lists;
    });
    this.connect('lists', this.actions.deleteList$, (state, id) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.deleteProp)(state.lists, `${id}`);
      }
      return state.lists;
    });
    this.connect('lists', this.actions.deleteMovieFromList$, (state, [movie, id]) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[id], {
            results: (state.lists[id].results || []).filter(m => m.id !== movie.id)
          })
        });
      }
      return state.lists;
    });
    this.hold(this.sideEffects$);
  }
  initialize() {
    return;
  }
  static #_ = this.ɵfac = function ListState_Factory(t) {
    return new (t || ListState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineInjectable"]({
    token: ListState,
    factory: ListState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 7866:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/grid-list/grid-list.component.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   GridListComponent: () => (/* binding */ GridListComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

const _c0 = [[["", 8, "ui-grid-list-item"]], "*"];
const _c1 = [".ui-grid-list-item", "*"];
class GridListComponent {
  static #_ = this.ɵfac = function GridListComponent_Factory(t) {
    return new (t || GridListComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: GridListComponent,
    selectors: [["ui-grid-list"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c1,
    decls: 2,
    vars: 0,
    template: function GridListComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojectionDef"](_c0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](1, 1);
      }
    },
    styles: ["[_nghost-%COMP%] {\n  display: grid;\n  grid-template-columns: repeat(auto-fit, minmax(10rem, 35rem));\n  gap: 4rem 2rem;\n  place-content: space-between space-evenly;\n  align-items: start;\n  margin: 4rem 0;\n  position: relative;\n  contain: layout;\n}\n@media only screen and (max-width: 600px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 23rem));\n    justify-content: space-around;\n    gap: 4rem 1.5rem;\n  }\n}\n@media only screen and (max-width: 500px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 18rem));\n    gap: 4rem 1rem;\n  }\n}\n[_nghost-%COMP%]  .ui-grid-list-item {\n  display: flex;\n  align-items: stretch;\n  flex-direction: column;\n  transition: transform 150ms cubic-bezier(0.4, 0, 0.2, 1) 0s;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:after {\n  content: \"\";\n  position: absolute;\n  z-index: -99;\n  top: 0px;\n  left: 0px;\n  width: 100%;\n  height: 100%;\n  transform: scaleY(0);\n  transform-origin: center top;\n  background-color: var(--palette-background-paper);\n  box-shadow: rgba(0, 0, 0, 0.2) 0 2px 1px -1px, rgba(0, 0, 0, 0.14) 0px 1px 1px 0px, rgba(0, 0, 0, 0.12) 0px 1px 3px 0px;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover {\n  transform: scale(1.03);\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover:after {\n  transform: scale(1);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9ncmlkLWxpc3QvZ3JpZC1saXN0LmNvbXBvbmVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBRUE7RUFDRSxhQUFBO0VBQ0EsNkRBQUE7RUFDQSxjQUFBO0VBQ0EseUNBQUE7RUFDQSxrQkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLGVBQUE7QUFERjtBQUdFO0VBVkY7SUFXSSw2REFBQTtJQUNBLDZCQUFBO0lBQ0EsZ0JBQUE7RUFBRjtBQUNGO0FBRUU7RUFoQkY7SUFpQkksNkRBQUE7SUFDQSxjQUFBO0VBQ0Y7QUFDRjtBQUVJO0VDdkJGLGFBQUE7RUR5Qkksb0JBQUE7RUFDQSxzQkFBQTtFQUNBLDJEQUFBO0FBQU47QUFFTTtFQUNFLFdBQUE7RUFDQSxrQkFBQTtFQUNBLFlBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0Esb0JBQUE7RUFDQSw0QkFBQTtFQUNBLGlEQUFBO0VBQ0EsdUhBQUE7QUFBUjtBQUlNO0VBQ0Usc0JBQUE7QUFGUjtBQUlRO0VBQ0UsbUJBQUE7QUFGViIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBkaXNwbGF5OiBncmlkO1xuICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAzNXJlbSkpO1xuICBnYXA6IDRyZW0gMnJlbTtcbiAgcGxhY2UtY29udGVudDogc3BhY2UtYmV0d2VlbiBzcGFjZS1ldmVubHk7XG4gIGFsaWduLWl0ZW1zOiBzdGFydDtcbiAgbWFyZ2luOiA0cmVtIDA7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgY29udGFpbjogbGF5b3V0O1xuXG4gIEBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogNjAwcHgpIHtcbiAgICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAyM3JlbSkpO1xuICAgIGp1c3RpZnktY29udGVudDogc3BhY2UtYXJvdW5kO1xuICAgIGdhcDogNHJlbSAxLjVyZW07XG4gIH1cblxuICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gICAgZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zOiByZXBlYXQoYXV0by1maXQsIG1pbm1heCgxMHJlbSwgMThyZW0pKTtcbiAgICBnYXA6IDRyZW0gMXJlbTtcbiAgfVxuXG4gICY6Om5nLWRlZXAge1xuICAgIC51aS1ncmlkLWxpc3QtaXRlbSB7XG4gICAgICBAaW5jbHVkZSBkLWZsZXg7XG4gICAgICBhbGlnbi1pdGVtczogc3RyZXRjaDtcbiAgICAgIGZsZXgtZGlyZWN0aW9uOiBjb2x1bW47XG4gICAgICB0cmFuc2l0aW9uOiB0cmFuc2Zvcm0gMTUwbXMgY3ViaWMtYmV6aWVyKDAuNCwgMCwgMC4yLCAxKSAwcztcblxuICAgICAgJjphZnRlciB7XG4gICAgICAgIGNvbnRlbnQ6ICcnO1xuICAgICAgICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gICAgICAgIHotaW5kZXg6IC05OTtcbiAgICAgICAgdG9wOiAwcHg7XG4gICAgICAgIGxlZnQ6IDBweDtcbiAgICAgICAgd2lkdGg6IDEwMCU7XG4gICAgICAgIGhlaWdodDogMTAwJTtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZVkoMCk7XG4gICAgICAgIHRyYW5zZm9ybS1vcmlnaW46IGNlbnRlciB0b3A7XG4gICAgICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gICAgICAgIGJveC1zaGFkb3c6IHJnYigwIDAgMCAvIDIwJSkgMCAycHggMXB4IC0xcHgsXG4gICAgICAgICAgcmdiKDAgMCAwIC8gMTQlKSAwcHggMXB4IDFweCAwcHgsIHJnYigwIDAgMCAvIDEyJSkgMHB4IDFweCAzcHggMHB4O1xuICAgICAgfVxuXG4gICAgICAmOmhvdmVyIHtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZSgxLjAzKTtcblxuICAgICAgICAmOmFmdGVyIHtcbiAgICAgICAgICB0cmFuc2Zvcm06IHNjYWxlKDEpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_account-list-page_account-list-page_component_ts.js.map