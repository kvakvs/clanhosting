class ApplicationController < ActionController::Base
  before_action :set_locale
  def set_locale
    loc = params[:locale] || session[:locale] || I18n.default_locale
    I18n.locale = session[:locale] = loc
  end

  def user_signed_in?
    session.has_key? :user_account
  end
  helper_method 'user_signed_in?'

  def forem_user
    if user_signed_in?
      if @current_user
        @current_user
      else
        u = User.new
        @current_user = u
      end
    else
      nil
    end
  end
  helper_method :forem_user

  # Prevent CSRF attacks by raising an exception.
  # For APIs, you may want to use :null_session instead.
  protect_from_forgery with: :exception
end
