class ApplicationController < ActionController::Base
  before_action :pre_set_locale
  before_action :pre_fetch_account_info
  before_action :pre_check_site_exists

  def pre_check_site_exists
    #session[:clan_forum_exists] = Forem::Category.exists?(:name => session[:user_clan].to_s)
    session[:clan_site_exists] = Site.exists?(:clan_id => session[:user_clan])
  end

  def pre_set_locale
    loc = params[:locale] || session[:locale] || I18n.default_locale
    I18n.locale = session[:locale] = loc
  end

  def pre_fetch_account_info
    if not session.has_key?(:account_info) and session[:user_account].is_a? Integer
      rpc = Rails.application.get_rpc
      acc_info = rpc.call.ch_user_api.get_session(session[:user_account])
      session[:account_info] = acc_info
    end
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
        u = User.new(:id => session[:user_account],)
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
