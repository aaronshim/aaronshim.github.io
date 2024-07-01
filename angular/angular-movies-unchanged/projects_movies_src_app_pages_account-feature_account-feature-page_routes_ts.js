"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_account-feature-page_routes_ts"],{

/***/ 8986:
/*!**************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/account-feature-page.routes.ts ***!
  \**************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/common/http */ 4860);
/* harmony import */ var _auth_tmdb_http_interceptor_feature__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../auth/tmdb-http-interceptor.feature */ 9419);


const ROUTES = [{
  path: '',
  providers: [(0,_angular_common_http__WEBPACK_IMPORTED_MODULE_1__.provideHttpClient)((0,_angular_common_http__WEBPACK_IMPORTED_MODULE_1__.withRequestsMadeViaParent)(), (0,_angular_common_http__WEBPACK_IMPORTED_MODULE_1__.withInterceptors)([_auth_tmdb_http_interceptor_feature__WEBPACK_IMPORTED_MODULE_0__.tmdbReadAccessInterceptor]))],
  children: [{
    path: 'my-lists',
    loadComponent: () => Promise.all(/*! import() */[__webpack_require__.e("common"), __webpack_require__.e("projects_movies_src_app_pages_account-feature_account-list-page_account-list-page_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./account-list-page/account-list-page.component */ 9689))
  }, {
    path: 'list/create',
    loadComponent: () => Promise.all(/*! import() */[__webpack_require__.e("default-node_modules_rx-angular_template_fesm2022_template-if_mjs"), __webpack_require__.e("default-projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_adapter_ts"), __webpack_require__.e("projects_movies_src_app_pages_account-feature_list-create-page_list-create-page_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./list-create-page/list-create-page.component */ 3375))
  }, {
    path: 'list/detail/:id',
    loadChildren: () => __webpack_require__.e(/*! import() */ "projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_routes_ts").then(__webpack_require__.bind(__webpack_require__, /*! ./list-detail-page/list-detail-page.routes */ 2287))
  }]
}];
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ROUTES);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_account-feature-page_routes_ts.js.map