class HomeController < ApplicationController
  def index
    Rails.application.bert_svc.call.derp.herp(1, 2, 3)
  end

  def wg_logged_in
    if params['status'] == 'ok'
      Rails.logger.info(request.env)
      # TODO: Custom Erlang-based session store
      session[:user_nickname]   = params['nickname']
      session[:user_account]    = params['account_id']
      session[:user_token]      = params['access_token']
      session[:user_expires_at] = Integer(params['expires_at']) + Time.now.to_i
      # TODO: Check user_token! do some srv_api_call(token, account)
      redirect_to root_path
    end
  end

  def wg_log_out
    session.clear
  end
end
