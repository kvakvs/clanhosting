class HomeController < ApplicationController
  def index
  end

  def wg_logged_in
    if params['status'] == 'ok'
      Rails.logger.info(request.env)
      # TODO: Custom Erlang-based session store
      session[:user_nickname]      = params['nickname']
      session[:user_account]       = params['account_id']
      session[:user_token] = token = params['access_token']
      session[:user_expires_at]    = Integer(params['expires_at']) + Time.now.to_i
      lang = 'ru'

      # Check user_token
      acc_id = Integer(params['account_id'])
      acc_info = Rails.application.get_rpc.call.ch_user_api.after_login(acc_id, token, lang)
      Rails.application.set_ses(acc_info)
      puts acc_info
      session[:user_clan] = acc_info['clan_id']

      redirect_to root_path
    end
  end

  def wg_log_out
    session.clear
  end
end
