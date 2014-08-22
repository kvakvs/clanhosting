class ApplicationController < ActionController::Base
  before_action :set_locale, :prefetch_account_info, :prefetch_clan_info
  def set_locale
    loc = params[:locale] || session[:locale] || I18n.default_locale
    I18n.locale = session[:locale] = loc
  end

  def prefetch_account_info
    if not session.has_key?(:account_info) and session[:user_account].is_a? Integer
      rpc = Rails.application.get_rpc
      acc_info = rpc.call.ch_user_api.get_session(session[:user_account])
      session[:account_info] = acc_info
    end
  end

  def prefetch_clan_info
    return session[:clan_info] if session.has_key?(:clan_info)
    return '' unless session[:user_clan].is_a? Integer

    rpc = Rails.application.get_rpc
    clan_info = rpc.call.ch_clan_api.clan_info(session[:user_clan],
                                               session[:user_token],
                                               'en')
    session[:clan_info] = clan_info
    session[:clan_forum_exists] = Forem::Category.exists?(:name => session[:user_clan].to_s)
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
