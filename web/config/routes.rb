Rails.application.routes.draw do

  get 'control_panel/index'

  get 'control_panel/newsfeed'

  get 'control_panel/acl'

  # This line mounts Forem's routes at /forums by default.
  # This means, any requests to the /forums URL of your application will go to Forem::ForumsController#index.
  # If you would like to change where this extension is mounted, simply change the :at option to something different.
  #
  # We ask that you don't use the :as option here, as Forem relies on it being the default of "forem"
  #mount Forem::Engine, :at => '/forums'

  # The priority is based upon order of creation: first created -> highest priority.
  # See how all your routes lay out with "rake routes".

  # You can have the root of your site routed with "root"
  root 'home#index'

  get 'home/wot_logged_in' => 'home#wot_logged_in'
  delete 'home/wot_log_out' => 'home#wot_log_out', :as => :wot_log_out

  get 'home/new_site' => 'home#new_site'
  post 'home/new_site' => 'home#new_site_create'

  get 'manage' => 'control_panel#index', :as => :control_panel

  get 'manage/newsfeed' => 'control_panel#newsfeed'
  get 'manage/newsfeed/add' => 'control_panel#newsfeed_add_form', :as => :manage_newsfeed_add_form
  post 'manage/newsfeed/add' => 'control_panel#newsfeed_add', :as => :manage_newsfeed_add

  get 'manage/acl' => 'control_panel#acl'

  # Example of regular route:
  #   get 'products/:id' => 'catalog#view'

  # Example of named route that can be invoked with purchase_url(id: product.id)
  #   get 'products/:id/purchase' => 'catalog#purchase', as: :purchase

  # Example resource route (maps HTTP verbs to controller actions automatically):
  #   resources :products

  # Example resource route with options:
  #   resources :products do
  #     member do
  #       get 'short'
  #       post 'toggle'
  #     end
  #
  #     collection do
  #       get 'sold'
  #     end
  #   end

  # Example resource route with sub-resources:
  #   resources :products do
  #     resources :comments, :sales
  #     resource :seller
  #   end

  # Example resource route with more complex sub-resources:
  #   resources :products do
  #     resources :comments
  #     resources :sales do
  #       get 'recent', on: :collection
  #     end
  #   end

  # Example resource route with concerns:
  #   concern :toggleable do
  #     post 'toggle'
  #   end
  #   resources :posts, concerns: :toggleable
  #   resources :photos, concerns: :toggleable

  # Example resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end
